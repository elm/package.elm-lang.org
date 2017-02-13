{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Memory.Timeline
  ( Timeline
  , read
  )
  where


import Prelude hiding (read)
import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Package as Pkg

import qualified Package.Overview as Overview



-- TIMELINE


type Timeline =
  Map.Map Time.NominalDiffTime (Pkg.Name, Pkg.Version)



-- INIT


read :: IO Timeline
read =
  do  users <- getSubDirs "packages"
      foldM addUser Map.empty users


addUser :: Timeline -> String -> IO Timeline
addUser timeline user =
  do  projects <- getSubDirs ("packages" </> user)
      foldM (addProject user) timeline projects


addProject :: String -> Timeline -> String -> IO Timeline
addProject user timeline project =
  do  let filePath = "packages" </> user </> project </> "overview.json"
      (Overview.Overview infos) <- Overview.read filePath
      let name = Pkg.Name (Text.pack user) (Text.pack project)
      return $ List.foldl' (addInfo name) timeline infos


addInfo :: Pkg.Name -> Timeline -> Overview.Info -> Timeline
addInfo name timeline (Overview.Info version date) =
  Map.insert date (name, version) timeline



-- DIRECTORIES


getSubDirs :: FilePath -> IO [FilePath]
getSubDirs dir =
  do  contents <- Dir.getDirectoryContents dir
      foldM addSubDir [] (map (dir </>) contents)


addSubDir :: [FilePath] -> FilePath -> IO [FilePath]
addSubDir dirs dir =
  do  exists <- Dir.doesDirectoryExist dir
      if exists && not (List.isPrefixOf "." dir)
        then return (dir : dirs)
        else return dirs
