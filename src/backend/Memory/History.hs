{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Memory.History
  ( History
  , Event
  , since
  , add
  , toDict
  , append
  , read
  )
  where


import Prelude hiding (init, null, read)
import qualified Data.Aeson as Json
import Data.Binary (get, put)
import Data.Binary.Get (Get, Decoder(..), isEmpty, runGetIncremental)
import Data.Binary.Put (Put, runPut)
import Data.ByteString (hGet, null)
import Data.ByteString.Lazy (hPut)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Monoid ((<>))
import System.Directory (doesFileExist)
import System.IO (Handle, IOMode(AppendMode, ReadMode), withBinaryFile)

import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)



-- HISTORY


data History =
  History
    { _size :: Int
    , _history :: [Event]
    }


data Event =
  Event
    { _name :: !Name
    , _vsn :: !Version
    }



-- SINCE


since :: Int -> History -> [Event]
since index (History size events) =
  take (size - index) events



-- ADD


add :: Name -> Version -> History -> History
add name version (History size events) =
  History (size + 1) (Event name version : events)



-- TO DICT


type Dict = Map.Map Name [Version]


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



-- BINARY


getEvent :: Get Event
getEvent =
  Event <$> get <*> get


getHistory :: Get History
getHistory =
  getHistoryHelp 0 []


getHistoryHelp :: Int -> [Event] -> Get History
getHistoryHelp size events =
  do  empty <- isEmpty
      case empty of
        True ->
          return (History size events)

        False ->
          do  event <- getEvent
              getHistoryHelp (size + 1) (event : events)


putEvent :: Event -> Put
putEvent (Event name version) =
  do  put name
      put version



-- APPEND


historyFile :: FilePath
historyFile =
  "history.dat"


append :: Name -> Version -> IO ()
append name version =
  withBinaryFile historyFile AppendMode $ \handle ->
    hPut handle (runPut (putEvent (Event name version)))



-- READ


read :: IO History
read =
  do  exists <- doesFileExist historyFile
      result <- if exists then readHelp else return Nothing
      case result of
        Just history ->
          return history

        Nothing ->
          build


readHelp :: IO (Maybe History)
readHelp =
  withBinaryFile historyFile ReadMode $ \handle ->
    chompHandle (runGetIncremental getHistory) handle


chompHandle :: Decoder a -> Handle -> IO (Maybe a)
chompHandle decoder handle =
  case decoder of
    Done _ _ value ->
      return (Just value)

    Fail _ _ _ ->
      return Nothing

    Partial k ->
      do  chunk <- hGet handle defaultChunkSize
          if null chunk
            then chompHandle (k Nothing) handle
            else chompHandle (k (Just chunk)) handle



-- REBUILD


build :: IO History
build =
  error "TODO"

