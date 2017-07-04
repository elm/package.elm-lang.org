{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Crawl
  ( Package(..)
  , upgradingPackages
  , newPackages
  , oldDir
  , newDir
  )
  where

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
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



-- UPGRADING PACKAGES


upgradingPackages :: Task.Task [Package]
upgradingPackages =
  crawl "packages"



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
      Maybe.catMaybes <$> traverse (crawlProject root user) projectNames


crawlProject :: FilePath -> String -> String -> Task.Task (Maybe Package)
crawlProject root user project =
  case Pkg.fromText (Text.pack user <> "/" <> Text.pack project) of
    Left _ ->
      do  liftIO $ putStrLn $ "bad name - " ++ user ++ "/" ++ project
          return Nothing

    Right pkg ->
      do  vsns <- getSubDirs (root </> user </> project)
          case traverse (Pkg.versionFromText . Text.pack) vsns of
            Right versions ->
              return (Just (Package pkg versions))

            Left _ ->
              throwError $ "Problem with " ++ user ++ "/" ++ project ++ " " ++ show vsns



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
