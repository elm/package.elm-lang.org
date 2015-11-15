{-# LANGUAGE OverloadedStrings #-}
module Routes where

import Control.Applicative
import Control.Monad.Except (ExceptT, forM_, runExceptT, liftIO, throwError, when)
import qualified Data.Aeson as Json
import qualified Data.Binary as Binary
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Text.Read as Read
import Snap.Core
import Snap.Util.FileServe
import Snap.Util.FileUploads
import System.Directory
import System.FilePath

import qualified Elm.Compiler.Module as Module
import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as Path
import qualified GitHub
import qualified NewPackageList
import qualified NativeWhitelist
import qualified PackageSummary as PkgSummary
import qualified ServeFile


packages :: Snap ()
packages =
    ifTop (ServeFile.filler "Elm Packages" (Module.Name ["Page","Catalog"]))
    <|> route [ (":user/:name", package) ]
    <|> serveDirectory "packages"


package :: Snap ()
package =
  do  user <- getParameter "user" Right
      name <- getParameter "name" Right
      let pkg = Pkg.Name user name

      route
        [ ("latest", redirectToLatest pkg)
        , (":version", servePackageInfo pkg)
        ]


servePackageInfo :: Pkg.Name -> Snap ()
servePackageInfo name =
  do  version <- getParameter "version" Pkg.versionFromString

      let pkgDir = packageRoot name version
      exists <- liftIO $ doesDirectoryExist pkgDir
      when (not exists) pass

      ifTop (ServeFile.pkgDocs name version Nothing)
        <|> serveModule name version


serveModule :: Pkg.Name -> Pkg.Version -> Snap ()
serveModule name version =
  do  request <- getRequest
      let potentialName = BS.unpack (rqPathInfo request)

      let docsDir = packageRoot name version </> "docs"
      exists <- liftIO $ doesFileExist (docsDir </> potentialName <.> "json")
      when (not exists) pass

      case Module.dehyphenate potentialName of
        Nothing -> pass
        Just moduleName ->
            ServeFile.pkgDocs name version (Just moduleName)


redirectToLatest :: Pkg.Name -> Snap ()
redirectToLatest name =
  do  rawVersions <- liftIO (getDirectoryContents (packageDirectory </> Pkg.toFilePath name))
      case Either.rights (map Pkg.versionFromString rawVersions) of
        [] ->
          httpStringError 404 $
            "Could not find any versions of package " ++ Pkg.toString name

        versions ->
          do  let latestVersion = last (List.sort versions)
              let url = "/packages/" ++ Pkg.toUrl name ++ "/" ++ Pkg.versionToString latestVersion ++ "/"
              request <- getRequest
              redirect (BS.append (BS.pack url) (rqPathInfo request))


-- DIRECTORIES

packageDirectory :: FilePath
packageDirectory =
    "packages"


packageRoot :: Pkg.Name -> Pkg.Version -> FilePath
packageRoot name version =
    packageDirectory </> Pkg.toFilePath name </> Pkg.versionToString version


documentationPath :: FilePath
documentationPath =
    "documentation.json"


-- REGISTER MODULES

register :: Snap ()
register =
  do  name <- getParameter "name" Pkg.fromString
      version <- getParameter "version" Pkg.versionFromString

      verifyVersion name version

      let directory = packageRoot name version
      liftIO (createDirectoryIfMissing True directory)
      uploadFiles directory
      description <- Desc.read (directory </> Path.description)

      result <-
          liftIO $ runExceptT $ do
            verifyWhitelist (Desc.natives description) (Desc.name description)
            splitDocs directory

      case result of
        Right () ->
          liftIO $ do
              PkgSummary.add description
              NewPackageList.addIfNew description

        Left err ->
          do  liftIO (removeDirectoryRecursive directory)
              httpStringError 400 err


verifyVersion :: Pkg.Name -> Pkg.Version -> Snap ()
verifyVersion name version =
  do  maybeVersions <- liftIO (PkgSummary.readVersionsOf name)
      case maybeVersions of
        Just localVersions
          | version `elem` localVersions ->
                httpStringError 400
                    ("Version " ++ Pkg.versionToString version ++ " has already been registered.")

        _ -> return ()

      publicVersions <- GitHub.getVersionTags name
      case version `elem` publicVersions of
        True -> return ()
        False ->
           httpStringError 400
              ("The tag " ++ Pkg.versionToString version ++ " has not been pushed to GitHub.")


verifyWhitelist :: Bool -> Pkg.Name -> ExceptT String IO ()
verifyWhitelist allowNatives name =
  case allowNatives of
    False -> return ()
    True ->
      do  whitelist <- liftIO NativeWhitelist.read
          case name `elem` whitelist of
            True -> return ()
            False -> throwError (whitelistError name)


whitelistError :: Pkg.Name -> String
whitelistError name =
    "You are trying to publish a project that has native modules. For now,\n\
    \any modules that use Native code must go through a formal review process to\n\
    \make sure the exposed API is pure and the Native code is absolutely\n\
    \necessary. Please open an issue with the title:\n\n"
    ++ "    \"Native review for " ++ Pkg.toString name ++ "\"\n\n"
    ++ "to begin the review process at the following address.\n"
    ++ "<https://github.com/elm-lang/package.elm-lang.org/issues>\n\n"
    ++ "The issue should link to the relevant repository and provide sufficient\n"
    ++ "context for evaluation."


-- UPLOADING FILES

uploadFiles :: FilePath -> Snap ()
uploadFiles directory =
    handleFileUploads "/tmp" defaultUploadPolicy perPartPolicy (handleParts directory)
  where
    perPartPolicy info =
      if Map.member (partFieldName info) filesForUpload
          then allowWithMaximumSize $ 2^(19::Int)
          else disallow


filesForUpload :: Map.Map BS.ByteString FilePath
filesForUpload =
  Map.fromList
  [ ("documentation", documentationPath)
  , ("description", Path.description)
  , ("readme", "README.md")
  ]


handleParts
    :: FilePath
    -> [(PartInfo, Either PolicyViolationException FilePath)]
    -> Snap ()

handleParts _dir [] =
  return ()

handleParts dir ((info, eitherPath) : parts) =
  case (eitherPath, Map.lookup (partFieldName info) filesForUpload) of
    (Right tempPath, Just targetPath) ->
      do  liftIO $ do
              contents <- BS.readFile tempPath
              BS.writeFile (dir </> targetPath) contents
          handleParts dir parts

    _ ->
      do  mapM (writePartError . snd) parts
          httpStringError 404 $
              "Files " ++ documentationPath ++ " and " ++ Path.description ++ " were not uploaded."


writePartError :: Either PolicyViolationException FilePath -> Snap ()
writePartError part =
    case part of
      Right _ ->
          return ()

      Left exception ->
          writeText (policyViolationExceptionReason exception)


splitDocs :: FilePath -> ExceptT String IO ()
splitDocs directory =
  do  json <- liftIO (LBS.readFile (directory </> documentationPath))
      case Json.decode json of
        Nothing -> throwError "The uploaded documentation is invalid."
        Just docs ->
          liftIO $
            forM_ (docs :: [Docs.Documentation]) $ \doc ->
              do  let name = Module.hyphenate (Docs.moduleName doc)
                  let docPath = directory </> "docs" </> name <.> "json"
                  createDirectoryIfMissing True (directory </> "docs")
                  LBS.writeFile docPath (Json.encode doc)



-- FETCH ALL AVAILABLE VERSIONS

versions :: Snap ()
versions =
  do  name <- getParameter "name" Pkg.fromString
      versions <- liftIO (PkgSummary.readVersionsOf name)
      writeLBS (Binary.encode versions)


-- UPDATE REMOTE PACKAGE CACHES

allPackages :: Snap ()
allPackages =
  do  maybeValue <- getParam "since"
      let maybeString = fmap BS.unpack maybeValue
      needsUpdate <-
          case Read.readMaybe =<< maybeString of
            Nothing -> return True
            Just remoteTime ->
              do  localTime <- liftIO (getModificationTime PkgSummary.allPackages)
                  return (remoteTime < localTime)

      case needsUpdate of
        False -> writeLBS "null"
        True -> serveFile PkgSummary.allPackages


-- FETCH RESOURCES

documentation :: Snap ()
documentation =
  fetch documentationPath

description :: Snap ()
description =
  fetch Path.description

fetch :: FilePath -> Snap ()
fetch filePath =
  do  name <- getParameter "name" Pkg.fromString
      version <- getParameter "version" Pkg.versionFromString

      let target = packageRoot name version </> filePath
      exists <- liftIO $ doesFileExist target

      case exists of
        True -> serveFile target
        False -> httpError 404 "That library and version is not registered."


-- HELPERS

getParameter :: BS.ByteString -> (String -> Either String a) -> Snap a
getParameter param fromString =
  do  maybeValue <- getParam param
      let notFoundMsg = "could not find parameter named " ++ BS.unpack param
      let eitherString = maybe (Left notFoundMsg) (Right . BS.unpack) maybeValue
      case fromString =<< eitherString of
        Right value ->
            return value

        Left problem ->
            httpError 400 $ BS.concat [ "problem with parameter '", param, "': ", BS.pack problem ]


httpStringError :: Int -> String -> Snap a
httpStringError code msg =
    httpError code (BS.pack msg)


httpError :: Int -> BS.ByteString -> Snap a
httpError code msg = do
  modifyResponse $ setResponseCode code
  writeBS msg
  finishWith =<< getResponse
