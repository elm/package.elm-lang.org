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


import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Time.Clock as Time
import qualified Data.Utf8 as Utf8
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Json.Encode as E
import qualified Package.Releases as Releases



-- HISTORY


data History =
  History
    { _size :: !Int
    , _history :: [Event]
    }


data Event =
  Event
    { _name :: !Pkg.Name
    , _vsn :: !V.Version
    }



-- HELPERS


add :: Pkg.Name -> V.Version -> History -> History
add pkg vsn (History size events) =
  History (size + 1) (Event pkg vsn : events)


since :: Int -> History -> [Event]
since index (History size events) =
  take (size - index) events


groupByName :: History -> Map.Map Pkg.Name [V.Version]
groupByName (History _ events) =
  let
    insert dict (Event pkg vsn) =
      Map.insertWith (++) pkg [vsn] dict
  in
    List.foldl' insert Map.empty events



-- JSON


encodeEvent :: Event -> E.Value
encodeEvent (Event pkg vsn) =
  E.chars $
    Pkg.toChars pkg ++ "@" ++ V.toChars vsn



-- LOAD


load :: IO History
load =
  do  times <- crawl
      let addEvent events (pkg, vsn) = Event pkg vsn : events
      return $ History (Map.size times) (Map.foldl addEvent [] times)


type TimeDict =
  Map.Map Time.NominalDiffTime (Pkg.Name, V.Version)


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
  let
    pkg =
      Pkg.Name (Utf8.fromChars user) (Utf8.fromChars project)

    addRelease d (Releases.Release vsn time) =
      Map.insert time (pkg, vsn) d
  in
  List.foldl' addRelease dict <$> Releases.read pkg



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
