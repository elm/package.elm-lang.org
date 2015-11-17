module Docs.Name where

import Html exposing (..)
import Html.Attributes exposing (..)
import String


type alias Canonical =
    { home : String
    , name : String
    }


toLink : Canonical -> Html
toLink ({home,name} as canonical) =
  if isPrim canonical then
    text name

  else
    let
      link =
        String.map (\c -> if c == '.' then '-' else c) home ++ "#" ++ name
    in
      a [href link] [text name]


isPrim : Canonical -> Bool
isPrim {home,name} =
  (&&) (home == "")
    <| name == "Bool"
    || name == "Char"
    || name == "String"
    || name == "Int"
    || name == "Float"
