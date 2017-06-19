{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Memory.History
  ( History
  , Event
  , load
  , add
  , since
  , encodeEvent
  , groupByName
  )
  where


import Control.Monad (foldM, forM)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Package as Pkg
import qualified Json.Encode as Encode
import qualified Memory.Releases as Releases



-- HISTORY


data History =
  History
    { _size :: !Int
    , _history :: [Event]
    }


data Event =
  Event
    { _name :: !Pkg.Name
    , _vsn :: !Pkg.Version
    }



-- HELPERS


add :: Pkg.Name -> Pkg.Version -> History -> History
add name version (History size events) =
  History (size + 1) (Event name version : events)


since :: Int -> History -> [Event]
since index (History size events) =
  take (size - index) events


groupByName :: History -> Map.Map Pkg.Name [Pkg.Version]
groupByName (History _ events) =
  let
    insert dict (Event name version) =
      Map.insertWith (++) name [version] dict
  in
    List.foldl' insert Map.empty events



-- JSON


encodeEvent :: Event -> Encode.Value
encodeEvent (Event name version) =
  Encode.text $
    Pkg.toText name <> "@" <> Pkg.versionToText version



-- LOAD


load :: IO History
load =
  do  times <- crawl
      let addEvent events (name, version) = Event name version : events
      return $ History (Map.size times) (Map.foldl addEvent [] times)


type TimeDict =
  Map.Map Time.NominalDiffTime (Pkg.Name, Pkg.Version)


crawl :: IO TimeDict
crawl =
  do  users <- getSubDirs "packages"
      foldM crawlUser Map.empty users


crawlUser :: TimeDict -> String -> IO TimeDict
crawlUser dict user =
  do  projects <- getSubDirs ("packages" </> user)
      foldM (crawlProject user) dict projects


crawlProject :: String -> TimeDict -> String -> IO TimeDict
crawlProject user dict project =
  do  let name = Pkg.Name (Text.pack user) (Text.pack project)

      let add d (Releases.Release version time) =
            Map.insert time (name, version) d

      List.foldl' add dict <$> Releases.read name



-- LOAD HELP


getSubDirs :: FilePath -> IO [FilePath]
getSubDirs dir =
  do  contents <- Dir.getDirectoryContents dir
      foldM (addSubDir dir) [] contents


addSubDir :: FilePath -> [FilePath] -> FilePath -> IO [FilePath]
addSubDir dir subs subDir =
  do  let path = dir </> subDir
      exists <- Dir.doesDirectoryExist path
      if exists && not (List.isPrefixOf "." subDir)
        then return (subDir : subs)
        else return subs
