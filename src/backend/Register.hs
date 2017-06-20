module Register (register) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Snap.Util.FileUploads


{- Be sure to check:

  - name
  - version
  - version is not already published
  - version is on github
  - sha matches github sha

-}

register :: Snap ()
register =
  do  sha <- getQueryParam "sha"
      name <- verifyName =<< getQueryParam "name"
      version <- verifyVersion name sha =<< getQueryParam "version"

      let directory = packageRoot name version
      liftIO (createDirectoryIfMissing True directory)
      uploadFiles directory

      -- TODO add to overview.json
      -- TODO add to Memory

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


verifyVersion :: Pkg.Name -> Text -> Text -> Snap Pkg.Version
verifyVersion name sha rawVersion =
  case Pkg.versionFromText rawVersion of
    Left problem ->
      httpStringError 400 $
        "I was given an invalid version: " ++ Text.unpack rawVersion ++ "\n\n" ++ problem

    Right version ->
      do  verifyIsNew version
          verifySha version sha
          return version


verifyIsNew :: Pkg.Version -> Snap ()
verifyIsNew version =
  do  localVersions <- error "TODO get all known versions. Read the overview?"
      when (elem version localVersions) $ httpStringError 400 $
        "Version " ++ Pkg.versionToString version ++ " has already been published."


verifySha :: Pkg.Version -> Text -> Snap ()
verifySha version sha =
  do  githubSha <- error "TODO get sha from github for version"
      when (sha /= githubSha) $ httpStringError 400 $
        "The commit tagged on github as " ++ Pkg.versionToString version ++ " is not the one I was expecting."



-- UPLOADING FILES


uploadFiles :: FilePath -> Snap ()
uploadFiles directory =
  do  results <- handleMultipart defaultUploadPolicy (handlePart directory)
      case Maybe.catMaybes results of
        [] ->
          return ()

        problems ->
          do  liftIO (removeDirectoryRecursive directory)
              httpStringError 404 $
                "Failure uploading your package:\n" ++ concatMap ("\n  - " ++) problems


handlePart :: FilePath -> PartInfo -> InputStream ByteString -> IO (Maybe String)
handlePart directory info stream =
  case partFieldName info of
    "hash" | partDisposition info == DispositionFormData ->
      boundedWrite directory "hash" stream

    "elm.json" | partDisposition info == DispositionFile ->
      boundedWrite directory "elm.json" stream

    "README.md" | partDisposition info == DispositionFile ->
      boundedWrite directory "README.md" stream

    "documentation.json" | partDisposition info == DispositionFile ->
      boundedWrite directory "documentation.json" stream

    name ->
      return $ Just $ "Did not recognize " ++ show name ++ " part in form-data"


boundedWrite :: FilePath -> FilePath -> InputStream ByteString -> IO (Maybe String)
boundedWrite directory path stream =
  IO.withBinaryFile (directory </> path) IO.WriteMode $ \handle ->
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

