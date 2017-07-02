{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Aeson ((.:))
import Data.Map (Map, (!))
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified System.Directory as Dir
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)



-- MAIN


main :: IO ()
main =
  do  set <- gatherLicenses
      print set


gatherLicenses :: IO (Set Text)
gatherLicenses =
  do  users <- getSubDirs "packages"
      Set.unions <$> mapM gatherProjects users


gatherProjects :: String -> IO (Set Text)
gatherProjects user =
  do  projects <- getSubDirs ("packages" </> user)
      Set.unions <$> mapM (gatherVersions user) projects


gatherVersions :: String -> String -> IO (Set Text)
gatherVersions user project =
  do  versions <- getSubDirs ("packages" </> user </> project)
      Set.fromList <$> mapM (readLicense user project) versions


readLicense :: String -> String -> String -> IO Text
readLicense user project version =
  do  let dir = "packages" </> user </> project </> version
      json <- BS.readFile (dir </> "elm-package.json")
      case Json.decode json of
        Nothing ->
          do  hPutStrLn stderr ("Problem at " ++ dir)
              exitFailure

        Just (Project oldLicense obj) ->
          do  BS.writeFile (dir </> "elm.json") (Json.encode obj)
              return oldLicense


data Project = Project Text Json.Object


instance Json.FromJSON Project where
  parseJSON =
    Json.withObject "Project" $ \obj ->
      do
          (Json.String license) <- obj .: "license"
          let spdx = conversions ! license

          (Json.String repo) <- obj .: "repository"
          let name = toName repo

          return $ Project license $
            HashMap.insert "type" (Json.String "package") $
            HashMap.delete "repository" $
            HashMap.insert "name" (Json.String name) $
            HashMap.insert "license" (Json.String spdx) $
            HashMap.insert "test-dependencies" (Json.Object HashMap.empty) $
              obj


toName :: Text -> Text
toName repository =
  Text.dropEnd 4 $
  Text.replace "http://github.com/" "" $
  Text.replace "https://github.com/" "" $
    repository



-- LICENSE CONVERTER


conversions :: Map Text Text
conversions =
  Map.fromList
    [ "" ==> "BSD-3-Clause"

    -- BSD
    , "BSD3" ==> "BSD-3-Clause"
    , "BSD3-Clause" ==> "BSD-3-Clause"
    , "BSD 3-Clause" ==> "BSD-3-Clause"
    , "BSD-3-Clause" ==> "BSD-3-Clause"
    , "BSD2" ==> "BSD-2-Clause"

    -- CORRECT ONES
    , "MIT" ==> "MIT"
    , "ISC" ==> "ISC"

    -- GPL
    , "AGPL-3" ==> "AGPL-3.0"
    , "GPLv3" ==> "GPL-3.0"

    -- APACHE
    , "Apache2.0" ==> "Apache-2.0"
    , "Apache 2.0" ==> "Apache-2.0"
    , "Apache-2.0" ==> "Apache-2.0"
    , "Apache License, version 2.0" ==> "Apache-2.0"
    , "Apache License, Version 2.0" ==> "Apache-2.0"

    -- MOZILLA
    , "MPL" ==> "MPL-2.0"
    , "MPL-2.0" ==> "MPL-2.0"
    , "MPL2" ==> "MPL-2.0"
    , "MPLv2" ==> "MPL-2.0"
    , "Mozilla Public License 2.0" ==> "MPL-2.0"

    -- NOT OSI
    , "CC0" ==> "CC0-1.0"
    , "WTFPL" ==> "WTFPL"
    , "WORDNET" ==> "WORDNET"
    ]


(==>) :: a -> b -> (a, b)
(==>) =
  (,)



-- DIRECTORIES


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
