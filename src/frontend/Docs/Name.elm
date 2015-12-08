module Docs.Name where

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Set
import String


type alias Canonical =
    { home : String
    , name : String
    }


type alias Dictionary =
    Dict.Dict String (Set.Set String)


toLink : Dictionary -> Canonical -> Html
toLink dict {home,name} =
  case Maybe.map (Set.member name) (Dict.get home dict) of
    Just True ->
      let
        link =
          String.map (\c -> if c == '.' then '-' else c) home ++ "#" ++ name
      in
        a [href link] [text name]

    _ ->
      text name
