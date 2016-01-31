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


pathTo : Canonical -> String
pathTo ({home,name} as canonical) =
    String.map (\c -> if c == '.' then '-' else c) home ++ "#" ++ name


basePathTo : String -> Canonical -> String
basePathTo basePath ({home,name} as canonical) =
    basePath ++ "/" ++ pathTo canonical


toLink : Dictionary -> Canonical -> Html
toLink dict ({home,name} as canonical) =
  case Maybe.map (Set.member name) (Dict.get home dict) of
    Just True ->
      let
        link = pathTo canonical
      in
        a [href link] [text name]

    _ ->
      text name


toBaseLink : String -> Dictionary -> Canonical -> Html
toBaseLink basePath dict ({home,name} as canonical) =
  case Maybe.map (Set.member name) (Dict.get home dict) of
    Just True ->
      let
        link = basePathTo basePath canonical
      in
        a [href link] [text name]

    _ ->
      text name
