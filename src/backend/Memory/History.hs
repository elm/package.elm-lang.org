{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Memory.History
  ( History
  , Event
  , since
  , add
  , toDict
  , fromTimeline
  )
  where


import qualified Data.Aeson as Json
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Monoid ((<>))

import qualified Elm.Package as Pkg

import qualified Memory.Timeline as Timeline



-- HISTORY


data History =
  History
    { _size :: Int
    , _history :: [Event]
    }


data Event =
  Event
    { _name :: !Pkg.Name
    , _vsn :: !Pkg.Version
    }



-- SINCE


since :: Int -> History -> [Event]
since index (History size events) =
  take (size - index) events



-- ADD


add :: Pkg.Name -> Pkg.Version -> History -> History
add name version (History size events) =
  History (size + 1) (Event name version : events)



-- TO DICT


type Dict = Map.Map Pkg.Name [Pkg.Version]


toDict :: History -> Dict
toDict (History _ events) =
  List.foldl' insert Map.empty events


insert :: Dict -> Event -> Dict
insert dict (Event name version) =
  Map.insertWith (++) name [version] dict



-- JSON


instance Json.ToJSON Event where
  toJSON (Event name version) =
    Json.String $
      Pkg.toText name <> "@" <> Pkg.versionToText version



-- FROM TIMELINE


fromTimeline :: Timeline.Timeline -> History
fromTimeline timeline =
  History (Map.size timeline) (Map.foldl addEvent [] timeline)


addEvent :: [Event] -> (Pkg.Name, Pkg.Version) -> [Event]
addEvent events (name, version) =
  Event name version : events
