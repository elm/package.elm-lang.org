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
        ctx =
          "Graphics.Collage"

        parseLink link =
          String.map (\c -> if c == '.' then '-' else c) link

        anchorHome =
          if home == ctx then
            "#"
          else
            home ++ "#"

        link =
           parseLink (anchorHome ++ name)

      in
        a [href link] [text name]

    _ ->
      text name
