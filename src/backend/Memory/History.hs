{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Memory.History
  ( History
  , Event(..)
  , since
  , add
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
import Data.Monoid ((<>))
import System.IO (Handle, IOMode(AppendMode, ReadMode), withBinaryFile)

import qualified Elm.Package as Pkg



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



-- WRITE


add :: FilePath -> Event -> IO ()
add filePath event =
  withBinaryFile filePath AppendMode $ \handle ->
    hPut handle (runPut (putEvent event))



-- READ


read :: FilePath -> IO (Maybe History)
read filePath =
  withBinaryFile filePath ReadMode $ \handle ->
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
