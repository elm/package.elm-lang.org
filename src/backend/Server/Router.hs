{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, GADTs, OverloadedStrings, UnboxedTuples #-}
module Server.Router
  ( Route, top, s, int, bytes, custom
  , (</>), map, oneOf
  , (==>)
  , serve
  )
  where


import Prelude hiding (length, map)
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeIndex)
import qualified Data.ByteString.Validate as BSV
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Word (Word8)
import qualified Snap.Core as Snap
import Snap.Core (Snap)



-- ROUTES


data Route a b where
  Top :: Route a a
  Exact :: ByteString -> Route a a
  Integer :: Route (Int -> a) a
  Custom :: (BS.ByteString -> Maybe a) -> Route (a -> b) b
  Slash :: Route a b -> Route b c -> Route a c
  Map :: a -> Route a b -> Route (b -> c) c
  OneOf :: [Route a b] -> Route a b


top :: Route a a
top =
  Top


s :: ByteString -> Route a a
s =
  Exact


int :: Route (Int -> a) a
int =
  Integer


bytes :: Route (BS.ByteString -> a) a
bytes =
  Custom Just


custom :: (BS.ByteString -> Maybe a) -> Route (a -> b) b
custom =
  Custom


(</>) :: Route a b -> Route b c -> Route a c
(</>) =
  Slash


infixr 7 </>


map :: a -> Route a b -> Route (b -> c) c
map =
  Map


oneOf :: [Route a b] -> Route a b
oneOf =
  OneOf


(==>) :: Route a b -> a -> Route (b -> c) c
(==>) route arg =
  Map arg route


infixl 5 ==>



-- SERVE


serve :: Route (Snap a -> Snap a) (Snap a) -> Snap a
serve route =
  do  request <- Snap.getRequest
      let path = Snap.rqPathInfo request
      case parse route path of
        Nothing ->
          Snap.pass

        Just snap ->
          do  Snap.putRequest (finishPath request)
              snap
          <|>
          do  Snap.putRequest request
              Snap.pass


finishPath :: Snap.Request -> Snap.Request
finishPath req =
  let
    path =
      BS.concat [ Snap.rqContextPath req, Snap.rqPathInfo req, "/" ]
  in
    req { Snap.rqContextPath = path, Snap.rqPathInfo = "" }



-- PARSE


parse :: Route (a -> a) a -> ByteString -> Maybe a
parse route bs =
  parseHelp $
    try route (State bs 0 (BS.length bs) id)


parseHelp :: [State a] -> Maybe a
parseHelp states =
  case states of
    [] ->
      Nothing

    State _ _ length value : otherStates ->
      if length == 0 then
        Just value
      else
        parseHelp otherStates


data State a =
  State
    { _url :: !ByteString
    , _offset :: !Int
    , _length :: !Int
    , _value :: a
    }


try :: Route a b -> State a -> [State b]
try route state@(State url offset length value) =
  case route of
    Top ->
      if length == 0 then [state] else []

    Exact byteString ->
      let
        (# newOffset, newLength #) =
          chompExact byteString url offset length
      in
        if newOffset == -1 then
          []

        else
          [State url newOffset newLength value]

    Integer ->
      let
        (# newOffset, newLength, number #) =
          chompInt url offset length
      in
        if newOffset <= offset then
          []

        else
          [State url newOffset newLength (value number)]

    Custom check ->
      let
        (# endOffset, newOffset, newLength #) =
          chompSegment url offset length
      in
        if endOffset == offset then
          []

        else
          let
            !subByteString =
              BS.take (endOffset - offset) (BS.drop offset url)
          in
          if BSV.isUtf8 subByteString
          then
            case check subByteString of
              Just nextValue -> [State url newOffset newLength (value nextValue)]
              Nothing        -> []
          else
            []

    Slash before after ->
      concatMap (try after) (try before state)

    Map subValue subParser ->
      List.map (mapHelp value) $
        try subParser (State url offset length subValue)

    OneOf routes ->
      concatMap (\p -> try p state) routes


mapHelp :: (a -> b) -> State a -> State b
mapHelp func (State url offset length value) =
  State url offset length (func value)


slash :: Word8
slash =
  fromIntegral (Char.ord '/')



-- CHOMP EXACT


chompExact :: ByteString -> ByteString -> Int -> Int -> (# Int, Int #)
chompExact small big offset length =
  let
    smallLen =
      BS.length small
  in
    if length < smallLen || smallLen == 0 then
      (# -1, length #)

    else
      if not (isSubString small big offset 0 smallLen) then
        (# -1, length #)

      else
        let
          !newOffset = offset + smallLen
          !newLength = length - smallLen
        in
          if newLength == 0 then
            (# newOffset, newLength #)

          else if unsafeIndex big newOffset == slash then
            (# newOffset + 1, newLength - 1 #)

          else
            (# -1, length #)


isSubString :: ByteString -> ByteString -> Int -> Int -> Int -> Bool
isSubString small big offset i smallLen =
  if i == smallLen then
    True

  else if unsafeIndex small i == unsafeIndex big (offset + i) then
    isSubString small big offset (i + 1) smallLen

  else
    False



-- CHOMP INT


chompInt :: ByteString -> Int -> Int -> (# Int, Int, Int #)
chompInt url offset length =
  if length == 0 then
    (# offset, length, 0 #)

  else
    let
      !word = unsafeIndex url offset
    in
      if 0x30 <= word && word <= 0x39 then
        chompIntHelp url (offset + 1) (length - 1) (fromIntegral word - 0x30)

      else
        (# offset, length, 0 #)


chompIntHelp :: ByteString -> Int -> Int -> Int -> (# Int, Int, Int #)
chompIntHelp url offset length n =
  if length == 0 then
    (# offset, length, n #)

  else
    let
      !word = unsafeIndex url offset
    in
      if 0x30 <= word && word <= 0x39 then
        chompIntHelp url (offset + 1) (length - 1) (n * 10 + (fromIntegral word - 0x30))

      else if word == slash then
        (# offset + 1, length - 1, n #)

      else
        (# offset, length, n #)



-- GET SEGMENT


chompSegment :: ByteString -> Int -> Int -> (# Int, Int, Int #)
chompSegment url offset length =
  if length == 0 then
    (# offset, offset, length #)

  else if unsafeIndex url offset == slash then
    (# offset, offset + 1, length - 1 #)

  else
    chompSegment url (offset + 1) (length - 1)
