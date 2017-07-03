{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Crawl
  ( Package(..)
  , crawl
  )
  where

import Control.Monad (foldM, forM)
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Package as Pkg



-- PACKAGES


data Package =
  Package
    { _pkg :: Pkg.Name
    , _vsns :: [Pkg.Version]
    }



-- CRAWL


crawl :: FilePath -> IO [Package]
crawl root =
  do  userNames <- getSubDirs root
      concat <$> traverse (crawlUser root) userNames


crawlUser :: FilePath -> String -> IO [Package]
crawlUser root user =
  do  projectNames <- getSubDirs (root </> user)
      forM projectNames $ \project ->
        do  vsns <- getSubDirs (root </> user </> project)
            either error return (toPackage user project vsns)


toPackage :: String -> String -> [String] -> Either String Package
toPackage user project versions =
  do  name <- Pkg.fromText (Text.pack user <> "/" <> Text.pack project)
      vsns <- traverse (Pkg.versionFromText . Text.pack) versions
      return (Package name vsns)



-- HELPERS


getSubDirs :: FilePath -> IO [FilePath]
getSubDirs dir =
  do  contents <- Dir.getDirectoryContents dir
      reverse <$> foldM (addSubDir dir) [] contents


addSubDir :: FilePath -> [FilePath] -> FilePath -> IO [FilePath]
addSubDir dir subs subDir =
  do  let path = dir </> subDir
      exists <- Dir.doesDirectoryExist path
      if exists && not (List.isPrefixOf "." subDir)
        then return (subDir : subs)
        else return subs
