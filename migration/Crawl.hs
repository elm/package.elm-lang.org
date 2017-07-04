{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Crawl
  ( Package(..)
  , newPackages
  , oldDir
  , newDir
  )
  where

import Control.Monad (foldM, forM)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Package as Pkg
import qualified Task



-- PACKAGES


data Package =
  Package
    { _pkg :: Pkg.Name
    , _vsns :: [Pkg.Version]
    }


oldDir :: Pkg.Name -> Pkg.Version -> FilePath
oldDir pkg vsn =
  "packages-18" </> Pkg.toFilePath pkg </> Pkg.versionToString vsn


newDir :: Pkg.Name -> Pkg.Version -> FilePath
newDir pkg vsn =
  "packages" </> Pkg.toFilePath pkg </> Pkg.versionToString vsn



-- NEW PACKAGES


newPackages :: Task.Task [Package]
newPackages =
  do  new <- crawl "packages"
      old <- crawl "packages-18"
      return (diff new old)


diff :: [Package] -> [Package] -> [Package]
diff new old =
  let
    destruct (Package pkg vsns) =
      (pkg, vsns)

    newDict =
      Map.fromList (map destruct new)

    isUnknown (Package pkg vsns) =
      case Map.lookup pkg newDict of
        Nothing ->
          True

        Just newVersions ->
          vsns /= newVersions
  in
    filter isUnknown old



-- CRAWL


crawl :: FilePath -> Task.Task [Package]
crawl root =
  do  userNames <- getSubDirs root
      concat <$> traverse (crawlUser root) userNames


crawlUser :: FilePath -> String -> Task.Task [Package]
crawlUser root user =
  do  projectNames <- getSubDirs (root </> user)
      forM projectNames $ \project ->
        do  vsns <- getSubDirs (root </> user </> project)
            either throwError return (toPackage user project vsns)


toPackage :: String -> String -> [String] -> Either String Package
toPackage user project versions =
  do  name <- Pkg.fromText (Text.pack user <> "/" <> Text.pack project)
      vsns <- traverse (Pkg.versionFromText . Text.pack) versions
      return (Package name vsns)



-- HELPERS


getSubDirs :: FilePath -> Task.Task [FilePath]
getSubDirs dir =
  liftIO $
  do  contents <- Dir.getDirectoryContents dir
      reverse <$> foldM (addSubDir dir) [] contents


addSubDir :: FilePath -> [FilePath] -> FilePath -> IO [FilePath]
addSubDir dir subs subDir =
  do  let path = dir </> subDir
      exists <- Dir.doesDirectoryExist path
      if exists && not (List.isPrefixOf "." subDir)
        then return (subDir : subs)
        else return subs
