module Docs.Version (Version, decoder, realMax, vsnToString) where

import Json.Decode as Json exposing (..)
import String


type alias Version = (Int, Int, Int)


decoder : Decoder Version
decoder =
  customDecoder string fromString


fromString : String -> Result String Version
fromString str =
  case all (List.map String.toInt (String.split "." str)) of
    Ok [major, minor, patch] ->
        Ok (major, minor, patch)

    _ ->
        Err (str ++ " is not a valid Elm version")


all : List (Result x a) -> Result x (List a)
all list =
  case list of
    [] ->
        Ok []

    x :: xs ->
        Result.map2 (::) x (all xs)


realMax : String -> List String -> Maybe String
realMax rawVsn allRawVsns =
  case Result.map2 (,) (fromString rawVsn) (all (List.map fromString allRawVsns)) of
    Ok (version, allVersions) ->
      let
        maxVersion =
          List.foldl max version allVersions
      in
        if version == maxVersion then
            Nothing

        else
            Just (vsnToString maxVersion)

    _ ->
        Nothing


vsnToString : Version -> String
vsnToString (major, minor, patch) =
  toString major ++ "." ++ toString minor ++ "." ++ toString patch

