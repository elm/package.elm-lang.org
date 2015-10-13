module Docs.Name where

import Html exposing (..)
import Html.Attributes exposing (..)
import String


type alias Canonical =
    { home : String
    , name : String
    }


toLink : Canonical -> Html
toLink {home,name} =
  let
    link =
      String.map (\c -> if c == '.' then '-' else c) home ++ "#" ++ name
  in
    a [href link] [text name]



