{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Package.Register
  ( register
  )
  where


import qualified Control.Exception as Ex
import Control.Monad ((>=>), when)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Validate as BSV
import qualified Data.Either as Either
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Snap.Core as Snap
import qualified Snap.Util.FileUploads as Snap
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))
import qualified System.IO.Streams.ByteString as SB
import qualified System.IO.Streams.Core as SC
import qualified System.IO.Streams.File as SF
import qualified System.IO.Streams.Zlib as SZ

import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Json.Decode as D
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Json.String as Json
import qualified Parse.Primitives as P

import qualified GitHub
import qualified Memory
import qualified Package.Path as Path
import qualified Package.Releases as Releases
import qualified Server.Error as Error


{- Be sure to check:

  - name
  - version
  - version is not already published
  - version is on github
  - commit hash matches github commit hash

-}

register :: GitHub.Token -> Memory.Memory -> Snap.Snap ()
register token memory =
  do  commitHash <- BSC.unpack <$> getQueryParam "commit-hash"
      pkg <- verifyName =<< getQueryParam "name"
      vsn <- verifyVersion token memory pkg commitHash =<< getQueryParam "version"

      time <- liftIO Time.getPOSIXTime

      outline <- uploadFiles pkg vsn time

      liftIO $ Releases.add pkg vsn time
      Memory.addPackage memory outline

      return ()


getQueryParam :: BS.ByteString -> Snap.Snap BS.ByteString
getQueryParam param =
  do  maybeValue <- Snap.getParam param
      case maybeValue of
        Nothing ->
          Error.string 400 $
            "I need a `" ++ BSC.unpack param ++ "` query parameter."

        Just bytes ->
          if BSV.isUtf8 bytes
          then return bytes
          else
            Error.bytestring 400 $ BS.concat $
              [ "The value of query parameter `"
              , param, "` is not valid UTF-8: ", bytes
              ]



-- VERIFY NAME


verifyName :: BS.ByteString -> Snap.Snap Pkg.Name
verifyName pkgBytes =
  case P.fromByteString Pkg.parser (,) pkgBytes of
    Right pkg ->
      return pkg

    Left _ ->
      Error.bytestring 400 $
        "I am seeing an invalid package name: " <> pkgBytes



-- VERIFY VERSION


verifyVersion :: GitHub.Token -> Memory.Memory -> Pkg.Name -> String -> BS.ByteString -> Snap.Snap V.Version
verifyVersion token memory pkg commitHash vsnBytes =
  case P.fromByteString V.parser (,) vsnBytes of
    Left _ ->
      Error.bytestring 400 $
        "I was given an invalid version: " <> vsnBytes

    Right vsn ->
      do  verifyIsNew memory pkg vsn
          verifyTag token pkg vsn commitHash
          return vsn


verifyIsNew :: Memory.Memory -> Pkg.Name -> V.Version -> Snap.Snap ()
verifyIsNew memory pkg vsn =
  do  pkgs <- Memory.getPackages memory
      case Map.lookup pkg pkgs of
        Nothing ->
          return ()

        Just (Memory.Summary versions _ _) ->
          when (elem vsn versions) $ Error.string 400 $
            "Version " ++ V.toChars vsn ++ " has already been published."


verifyTag :: GitHub.Token -> Pkg.Name -> V.Version -> String -> Snap.Snap ()
verifyTag token pkg vsn commitHash =
  do  githubHash <- getCommitHash token pkg vsn
      when (commitHash /= githubHash) $ Error.string 400 $
        "The commit tagged on github as " ++ V.toChars vsn ++ " is not the one I was expecting."


getCommitHash :: GitHub.Token -> Pkg.Name -> V.Version -> Snap.Snap String
getCommitHash token pkg vsn =
  do  response <- liftIO $ GitHub.fetchPath token $
        "/repos/" ++ Pkg.toUrl pkg ++ "/git/refs/tags/" ++ V.toChars vsn

      case response of
        Left _ ->
          Error.bytestring 500 "Request to GitHub API failed."

        Right body ->
          case D.fromByteString tagDecoder (LBS.toStrict body) of
            Right hash ->
              return (Json.toChars hash)

            Left _ ->
              Error.bytestring 500 "Request to GitHub API failed due to unexpected JSON."


tagDecoder :: D.Decoder e Json.String
tagDecoder =
  D.field "object" (D.field "sha" D.string)



-- UPLOADING FILES


{-| After a successful upload of tom/queue, the following files will be created:

    packages/tom/queue/2.0.0/
        README.md.gz
        elm.json.gz
        docs.json.gz
        endpoint.json
        time.dat

-}
uploadFiles :: Pkg.Name -> V.Version -> Time.POSIXTime -> Snap.Snap Outline.PkgOutline
uploadFiles pkg vsn time =
  do  let dir = Path.directory pkg vsn
      liftIO (Dir.createDirectoryIfMissing True dir)
      results <- Snap.handleMultipart Snap.defaultUploadPolicy (handlePart pkg vsn dir)
      case Either.partitionEithers results of
        ([], files) ->
          do  requireFile pkg vsn "README.md" files
              requireFile pkg vsn "elm.json" files
              requireFile pkg vsn "docs.json" files
              requireHash pkg vsn dir files
              bytes <- liftIO $ readElmJson dir
              case D.fromByteString Outline.decoder bytes of
                Left _ ->
                  revert pkg vsn $ "Invalid content in elm.json file."

                Right (Outline.App _) ->
                  revert pkg vsn $ "Invalid content in elm.json file."

                Right (Outline.Pkg outline) ->
                  do  liftIO $ writeFile (dir </> "time.dat") (show (floor time :: Integer))
                      return outline

        (problems, _) ->
          revert pkg vsn $ "Failure uploading your package:" ++ concatMap ("\n  - " ++) problems


requireFile :: Pkg.Name -> V.Version -> FilePath -> [FilePath] -> Snap.Snap ()
requireFile pkg vsn fileName uploadedFiles =
  if elem fileName uploadedFiles
  then return ()
  else revert pkg vsn $ "Malformed request. Missing the " ++ fileName ++ " file."


requireHash :: Pkg.Name -> V.Version -> FilePath -> [FilePath] -> Snap.Snap ()
requireHash pkg vsn dir uploadedFiles =
  if elem "endpoint.json" uploadedFiles
  then return ()
  else
    do  hash <- BSC.unpack <$> getQueryParam "github-hash"
        liftIO (writeEndpoint pkg vsn dir hash)


revert :: Pkg.Name -> V.Version -> String -> Snap.Snap a
revert pkg vsn details =
  do  liftIO (Path.removeDirectory pkg vsn)
      Error.string 400 details


handlePart :: Pkg.Name -> V.Version -> FilePath -> Snap.PartInfo -> SC.InputStream BS.ByteString -> IO (Either String FilePath)
handlePart pkg vsn dir info stream =
  case Snap.partFieldName info of
    "README.md" ->
      boundedGzipAndWrite dir "README.md" stream

    "elm.json" ->
      boundedGzipAndWrite dir "elm.json" stream

    "docs.json" ->
      boundedGzipAndWrite dir "docs.json" stream

    "github-hash" ->
      boundedWriteEndpoint pkg vsn dir stream

    path ->
      return $ Left $ "Did not recognize " ++ show path ++ " part in form-data"


readElmJson :: FilePath -> IO BS.ByteString
readElmJson dir =
  SF.withFileAsInput (dir </> "elm.json.gz") (SZ.gunzip >=> unpack "")
  where
    unpack bytes input =
      do  maybeChunk <- SC.read input
          case maybeChunk of
            Just chunk -> unpack (bytes <> chunk) input
            Nothing    -> return bytes



-- BOUNDED GZIP AND WRITE


boundedGzipAndWrite :: FilePath -> FilePath -> SC.InputStream BS.ByteString -> IO (Either String FilePath)
boundedGzipAndWrite dir path input =
  Ex.handle (exceededMaxBytes path) $
  SF.withFileAsOutput (dir </> path <.> "gz") $ \output ->
    do  boundedInput <- SB.throwIfProducesMoreThan 512000 input
        gzipper <- SZ.gzip (SZ.CompressionLevel 9) output
        SC.connect boundedInput gzipper
        return (Right path)


exceededMaxBytes :: FilePath -> SB.TooManyBytesReadException -> IO (Either String a)
exceededMaxBytes path _ =
  return $ Left $
    "Your " ++ path ++ " is too big. Must be less than 512kb.\n\
    \Let us know if this limit is too low!"



-- BOUNDED WRITE ENDPOINTS


boundedWriteEndpoint :: Pkg.Name -> V.Version -> FilePath -> SC.InputStream BS.ByteString -> IO (Either String FilePath)
boundedWriteEndpoint pkg vsn dir stream =
    boundedRead 0 ""
  where
    boundedRead size bytes =
      if 1000 < size then
        return $ Left "The hash of your assets should not be more than 1kb"

      else
        do  maybeChunk <- SC.read stream
            case maybeChunk of
              Just chunk ->
                boundedRead (BS.length chunk + size) (bytes <> chunk)

              Nothing ->
                if BSV.isUtf8 bytes
                then
                  do  writeEndpoint pkg vsn dir (BSC.unpack bytes)
                      return $ Right "endpoint.json"
                else
                  return $ Left "The hash of your assets is malformed"


writeEndpoint :: Pkg.Name -> V.Version -> FilePath -> String -> IO ()
writeEndpoint pkg vsn dir hash =
  E.writeUgly (dir </> "endpoint.json") $
    E.object
      [ "url" ==> E.chars (toGithubUrl pkg vsn)
      , "hash" ==> E.chars hash
      ]


toGithubUrl :: Pkg.Name -> V.Version -> String
toGithubUrl pkg vsn =
  "https://github.com/" ++ Pkg.toUrl pkg ++ "/zipball/" ++ V.toChars vsn ++ "/"
