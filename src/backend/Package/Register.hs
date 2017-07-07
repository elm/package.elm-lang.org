{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Package.Register (register) where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Either as Either
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Snap.Core as Snap
import qualified Snap.Util.FileUploads as Snap
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Streams as Stream

import qualified Elm.Package as Pkg
import qualified Http
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
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

register :: Http.Token -> Memory.Memory -> Snap.Snap ()
register token memory =
  do  name <- verifyName =<< getQueryParam "name"
      commitHash <- getQueryParam "commit-hash"
      version <- verifyVersion token memory name commitHash =<< getQueryParam "version"

      time <- liftIO Time.getPOSIXTime

      uploadFiles name version time

      liftIO $ Releases.add name version time
      Memory.addPackage memory name version

      return ()


getQueryParam :: BS.ByteString -> Snap.Snap Text
getQueryParam param =
  do  maybeValue <- Snap.getParam param
      case maybeValue of
        Nothing ->
          Error.string 400 $ "I need a `" ++ BS.unpack param ++ "` query parameter."

        Just bits ->
          case Text.decodeUtf8' bits of
            Left _ ->
              Error.string 400 $ "The value of query parameter `" ++ BS.unpack param ++ "` is not valid UTF-8."

            Right value ->
              return value



-- VERIFY NAME


verifyName :: Text -> Snap.Snap Pkg.Name
verifyName rawName =
  case Pkg.fromText rawName of
    Right name ->
      return name

    Left problem ->
       Error.string 400 (badNameMessage rawName problem)


badNameMessage :: Text -> String -> String
badNameMessage name problem =
  unlines
    [ "The name `" ++ Text.unpack name ++ "` has a problem:"
    , ""
    , "    " ++ problem
    , ""
    , "Change the name of your REPO on GitHub and it should be fine."
    , ""
    , "NOTE: Your user name is fine. The rules are only for repo names!"
    ]



-- VERIFY VERSION


verifyVersion :: Http.Token -> Memory.Memory -> Pkg.Name -> Text -> Text -> Snap.Snap Pkg.Version
verifyVersion token memory name commitHash rawVersion =
  case Pkg.versionFromText rawVersion of
    Left problem ->
      Error.string 400 $
        "I was given an invalid version: " ++ Text.unpack rawVersion ++ "\n\n" ++ problem

    Right version ->
      do  verifyIsNew memory name version
          verifyTag token name version commitHash
          return version


verifyIsNew :: Memory.Memory -> Pkg.Name -> Pkg.Version -> Snap.Snap ()
verifyIsNew memory name vsn =
  do  pkgs <- Memory.getPackages memory
      case Map.lookup name pkgs of
        Nothing ->
          return ()

        Just versions ->
          when (elem vsn versions) $ Error.string 400 $
            "Version " ++ Pkg.versionToString vsn ++ " has already been published."


verifyTag :: Http.Token -> Pkg.Name -> Pkg.Version -> Text -> Snap.Snap ()
verifyTag token name version commitHash =
  do  githubHash <- getCommitHash token name version
      when (commitHash /= githubHash) $ Error.string 400 $
        "The commit tagged on github as " ++ Pkg.versionToString version ++ " is not the one I was expecting."


getCommitHash :: Http.Token -> Pkg.Name -> Pkg.Version -> Snap.Snap Text
getCommitHash token name version =
  do  response <- liftIO $ Http.fetchGithub token $
        "/repos/" ++ Pkg.toUrl name ++ "/git/refs/tags/" ++ Pkg.versionToString version

      case response of
        Left _ ->
          Error.bytestring 500 "Request to GitHub API failed."

        Right body ->
          case Decode.parse tagDecoder body of
            Right hash ->
              return hash

            Left _ ->
              Error.bytestring 500 "Request to GitHub API failed due to unexpected JSON."


tagDecoder :: Decode.Decoder Text
tagDecoder =
  Decode.at ["object","sha"] Decode.text



-- UPLOADING FILES


{-| After a successful upload of tom/queue, the following files will be created:

    packages/tom/queue/2.0.0/
        README.md
        elm.json
        docs.json
        endpoint.json
        time.dat

-}
uploadFiles :: Pkg.Name -> Pkg.Version -> Time.POSIXTime -> Snap.Snap ()
uploadFiles name version time =
  do  let dir = Path.directory name version
      liftIO (Dir.createDirectoryIfMissing True dir)
      results <- Snap.handleMultipart Snap.defaultUploadPolicy (handlePart name version dir)
      case Either.partitionEithers results of
        ([], files) ->
          if Set.fromList files == requiredFiles then
            liftIO $ writeFile (dir </> "time.dat") (show (floor time :: Integer))
          else
            do  liftIO (Dir.removeDirectoryRecursive dir)
                Error.string 404 "Malformed request. Missing some metadata files."

        (problems, _) ->
          do  liftIO (Dir.removeDirectoryRecursive dir)
              Error.string 404 $
                "Failure uploading your package:" ++ concatMap ("\n  - " ++) problems


requiredFiles :: Set.Set FilePath
requiredFiles =
  Set.fromList [ "README.md", "elm.json", "docs.json", "endpoint.json" ]


handlePart :: Pkg.Name -> Pkg.Version -> FilePath -> Snap.PartInfo -> Stream.InputStream BS.ByteString -> IO (Either String FilePath)
handlePart name version dir info stream =
  case Snap.partFieldName info of
    "README.md" ->
      boundedWrite dir "README.md" stream

    "elm.json" ->
      boundedWrite dir "elm.json" stream

    "docs.json" ->
      boundedWrite dir "docs.json" stream

    "github-hash" ->
      writeEndpoint name version dir stream

    path ->
      return $ Left $ "Did not recognize " ++ show path ++ " part in form-data"



-- WRITE FILE


boundedWrite :: FilePath -> FilePath -> Stream.InputStream BS.ByteString -> IO (Either String FilePath)
boundedWrite dir path stream =
  IO.withBinaryFile (dir </> path) IO.WriteMode $ \handle ->
    boundedWriteHelp path handle 0 stream


boundedWriteHelp :: FilePath -> IO.Handle -> Int -> Stream.InputStream BS.ByteString -> IO (Either String FilePath)
boundedWriteHelp path handle size stream =
  if size < 128000 then
    do  maybeChunk <- Stream.read stream
        case maybeChunk of
          Nothing ->
            return (Right path)

          Just chunk ->
            do  BS.hPut handle chunk
                boundedWriteHelp path handle (BS.length chunk + size) stream
  else
    return $ Left $
      "Your " ++ path ++ " is too big. Must be less than 128kb. Let us know if this limit is too low!"



-- WRITE ENDPOINTS


writeEndpoint :: Pkg.Name -> Pkg.Version -> FilePath -> Stream.InputStream BS.ByteString -> IO (Either String FilePath)
writeEndpoint name version dir stream =
    boundedRead 0 ""
  where
    boundedRead size bits =
      if 1000 < size then
        return $ Left "The hash of your assets should not be more than 1kb"

      else
        do  maybeChunk <- Stream.read stream
            case maybeChunk of
              Just chunk ->
                boundedRead (BS.length chunk + size) (bits <> chunk)

              Nothing ->
                case Text.decodeUtf8' bits of
                  Left _ ->
                    return $ Left "The hash of your assets is malformed"

                  Right hash ->
                    do  Encode.writeUgly (dir </> "endpoint.json") $
                          Encode.object
                            [ ("url", Encode.string (toGithubUrl name version))
                            , ("hash", Encode.text hash)
                            ]

                        return $ Right "endpoint.json"


toGithubUrl :: Pkg.Name -> Pkg.Version -> String
toGithubUrl name version =
  "https://github.com/" ++ Pkg.toUrl name ++ "/zipball/" ++ Pkg.versionToString version ++ "/"
