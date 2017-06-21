{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Package.Register (register) where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
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
import qualified GitHub
import qualified Json.Decode as Decode
import qualified Memory
import qualified Package.Path as Path
import qualified Package.Releases as Releases
import qualified Server.Error as Error


{- Be sure to check:

  - name
  - version
  - version is not already published
  - version is on github
  - sha matches github sha

-}

register :: GitHub.Token -> Memory.Memory -> Snap.Snap ()
register token memory =
  do  sha <- getQueryParam "sha"
      name <- verifyName =<< getQueryParam "name"
      version <- verifyVersion token memory name sha =<< getQueryParam "version"

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


verifyVersion :: GitHub.Token -> Memory.Memory -> Pkg.Name -> Text -> Text -> Snap.Snap Pkg.Version
verifyVersion token memory name sha rawVersion =
  case Pkg.versionFromText rawVersion of
    Left problem ->
      Error.string 400 $
        "I was given an invalid version: " ++ Text.unpack rawVersion ++ "\n\n" ++ problem

    Right version ->
      do  verifyIsNew memory name version
          verifySha token name version sha
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


verifySha :: GitHub.Token -> Pkg.Name -> Pkg.Version -> Text -> Snap.Snap ()
verifySha token name version sha =
  do  githubSha <- getSha token name version
      when (sha /= githubSha) $ Error.string 400 $
        "The commit tagged on github as " ++ Pkg.versionToString version ++ " is not the one I was expecting."


getSha :: GitHub.Token -> Pkg.Name -> Pkg.Version -> Snap.Snap Text
getSha token name version =
  do  response <- liftIO $ GitHub.fetch token $
        "/repos/" ++ Pkg.toFilePath name ++ "/git/refs/tags/" ++ Pkg.versionToString version

      case response of
        Left _ ->
          Error.bytestring 500 "Request to GitHub API failed."

        Right body ->
          case Decode.parse tagDecoder body of
            Right sha ->
              return sha

            Left _ ->
              Error.bytestring 500 "Bad JSON from GitHub"


tagDecoder :: Decode.Decoder Text
tagDecoder =
  Decode.at ["object","sha"] Decode.text



-- UPLOADING FILES


{-| After a successful upload of tom/queue, the following files will be created:

    packages/tom/queue/2.0.0/
        elm.json
        README.md
        documentation.json
        commit-hash
        time

-}
uploadFiles :: Pkg.Name -> Pkg.Version -> Time.POSIXTime -> Snap.Snap ()
uploadFiles name version time =
  do  let dir = Path.directory name version
      liftIO (Dir.createDirectoryIfMissing True dir)
      results <- Snap.handleMultipart Snap.defaultUploadPolicy (handlePart dir)
      case Maybe.catMaybes results of
        [] ->
          liftIO $ writeFile (dir </> "time") (show (floor time :: Integer))

        problems ->
          do  liftIO (Dir.removeDirectoryRecursive dir)
              Error.string 404 $
                "Failure uploading your package:\n" ++ concatMap ("\n  - " ++) problems


handlePart :: FilePath -> Snap.PartInfo -> Stream.InputStream BS.ByteString -> IO (Maybe String)
handlePart dir info stream =
  case Snap.partFieldName info of
    "commit-hash" | Snap.partDisposition info == Snap.DispositionFormData ->
      boundedWrite dir "commit-hash" stream

    "elm.json" | Snap.partDisposition info == Snap.DispositionFile ->
      boundedWrite dir "elm.json" stream

    "README.md" | Snap.partDisposition info == Snap.DispositionFile ->
      boundedWrite dir "README.md" stream

    "documentation.json" | Snap.partDisposition info == Snap.DispositionFile ->
      boundedWrite dir "documentation.json" stream

    name ->
      return $ Just $ "Did not recognize " ++ show name ++ " part in form-data"


boundedWrite :: FilePath -> FilePath -> Stream.InputStream BS.ByteString -> IO (Maybe String)
boundedWrite dir path stream =
  IO.withBinaryFile (dir </> path) IO.WriteMode $ \handle ->
    boundedWriteHelp path handle 0 stream


boundedWriteHelp :: FilePath -> IO.Handle -> Int -> Stream.InputStream BS.ByteString -> IO (Maybe String)
boundedWriteHelp path handle size stream =
  if maxSize < size then
    return $ Just $
      "Your " ++ path ++ " is too big. Must be less than 128kb. Let us know if this limit is too low!"

  else
    do  maybeChunk <- Stream.read stream
        case maybeChunk of
          Nothing ->
            return Nothing

          Just chunk ->
            do  BS.hPut handle chunk
                boundedWriteHelp path handle (BS.length chunk + size) stream


maxSize :: Int
maxSize =
  2 ^ (17 :: Int)
