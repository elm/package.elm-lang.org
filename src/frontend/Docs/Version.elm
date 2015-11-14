module Docs.Version (Version, decode) where

import Json.Decode as Json exposing (..)


type alias Version = (Int, Int, Int)


decode : Decoder Version
decode =
  string `andThen` \str ->
    case all (map String.toInt (String.split "." str)) of
      Ok [major, minor, patch] ->
          Json.succeed (major, minor, patch)

      _ ->
          Json.fail (str ++ " is not a valid Elm version")


all : List (Result x a) -> Result x (List a)
all list =
  case list of
    [] ->
        Result.ok []

    x :: xs ->
        Result.map2 (::) x (all xs)