module Package.Register (register) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Snap.Util.FileUploads

import qualified GitHub
import qualified Package.Path as Path
import qualified Package.Releases as Releases


{- Be sure to check:

  - name
  - version
  - version is not already published
  - version is on github
  - sha matches github sha

-}

register :: Memory -> Snap ()
register memory =
  do  sha <- getQueryParam "sha"
      name <- verifyName =<< getQueryParam "name"
      (version, time) <- verifyVersion name sha memory =<< getQueryParam "version"

      uploadFiles name version time

      liftIO $ Releases.add name version time
      Memory.addPackage memory name version

      return ()


getQueryParam :: BS.ByteString -> Snap Text
getQueryParam param =
  do  maybeValue <- getParam param
      case maybeValue of
        Nothing ->
          httpError 400 $ "I need a `" ++ BS.unpack param ++ "` query parameter."

        Just bits ->
          case Text.decodeUtf8' bits of
            Left _ ->
              httpError 400 $ "The value of query parameter `" ++ BS.unpack param ++ "` is not valid UTF-8."

            Right value ->
              return value



-- VERIFY NAME


verifyName :: Text -> Snap Pkg.Name
verifyName rawName =
  case Pkg.fromText rawName of
    Right name ->
      return name

    Left problem ->
       httpStringError 400 (badNameMessage rawName problem)


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


verifyVersion :: Pkg.Name -> Text -> Text -> Memory -> Snap (Pkg.Version, Time.NominalDiffTime)
verifyVersion name sha rawVersion memory =
  case Pkg.versionFromText rawVersion of
    Left problem ->
      httpStringError 400 $
        "I was given an invalid version: " ++ Text.unpack rawVersion ++ "\n\n" ++ problem

    Right version ->
      do  verifyIsNew memory name version
          time <- verifySha version sha
          return (version, time)


verifyIsNew :: Memory -> Pkg.Name -> Pkg.Version -> Snap ()
verifyIsNew memory name vsn =
  do  pkgs <- Memory.getPackages memory
      case Map.lookup name pkgs of
        Nothing ->
          return ()

        Just versions ->
          when (elem vsn versions) $ httpStringError 400 $
            "Version " ++ Pkg.versionToString vsn ++ " has already been published."


verifySha :: Token -> Pkg.Name -> Pkg.Version -> Text -> Snap ()
verifySha token name version sha =
  do  githubSha <- GitHub.getSha token name version
      when (sha /= githubSha) $ httpStringError 400 $
        "The commit tagged on github as " ++ Pkg.versionToString version ++ " is not the one I was expecting."



-- UPLOADING FILES


{-| After a successful upload of tom/queue, the following files will be created:

    packages/tom/queue/2.0.0/
        elm.json
        README.md
        documentation.json
        commit-hash
        time

-}
uploadFiles :: Pkg.Name -> Pkg.Version -> Time.NominalDiffTime -> Snap ()
uploadFiles name version time =
  do  let dir = Path.dir name version
      liftIO (createDirectoryIfMissing True dir)
      results <- handleMultipart defaultUploadPolicy (handlePart dir)
      case Maybe.catMaybes results of
        [] ->
          liftIO $ writeFile (dir </> "time") (show (floor time))

        problems ->
          do  liftIO (removeDirectoryRecursive dir)
              httpStringError 404 $
                "Failure uploading your package:\n" ++ concatMap ("\n  - " ++) problems


handlePart :: FilePath -> PartInfo -> InputStream ByteString -> IO (Maybe String)
handlePart dir info stream =
  case partFieldName info of
    "commit-hash" | partDisposition info == DispositionFormData ->
      boundedWrite dir "commit-hash" stream

    "elm.json" | partDisposition info == DispositionFile ->
      boundedWrite dir "elm.json" stream

    "README.md" | partDisposition info == DispositionFile ->
      boundedWrite dir "README.md" stream

    "documentation.json" | partDisposition info == DispositionFile ->
      boundedWrite dir "documentation.json" stream

    name ->
      return $ Just $ "Did not recognize " ++ show name ++ " part in form-data"


boundedWrite :: FilePath -> FilePath -> InputStream ByteString -> IO (Maybe String)
boundedWrite dir path stream =
  IO.withBinaryFile (dir </> path) IO.WriteMode $ \handle ->
    boundedWriteHelp path handle 0 stream


boundedWriteHelp :: FilePath -> Handle -> Int -> InputStream ByteString -> IO (Maybe String)
boundedWriteHelp path handle bits stream =
  if bits > 2^17 then
    return $ Just $
      "Your " ++ path ++ " is too big. Must be less than 128kb. Let us know if this limit is too low!"

  else
    do  maybeChunk <- Stream.read
        case maybeChunk of
          Nothing ->
            return Nothing

          Just chunk ->
            do  BS.hPut handle chunk
                boundedWriteHelp path handle (BS.length chunk + size) stream

