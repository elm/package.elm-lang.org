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


type alias Context =
    { current : String
    , available : Set.Set String
    }


toLink : Context -> Canonical -> Html
toLink ctx {home,name} =
  if Set.member name ctx.available then
    let
      link =
        (anchorContext ctx home) ++ name
          |> parseLink

    in
      a [href link] [text name]

  else
    text (qualifiedType home name)


parseLink : String -> String
parseLink link =
  String.map (\c -> if c == '.' then '-' else c) link


anchorContext : Context -> String -> String
anchorContext {current} home =
  if home == current then
    "#"
  else
    home ++ "#"


qualifiedType : String -> String -> String
qualifiedType moduleName tipe =
  if String.isEmpty moduleName then
    tipe
  else
    moduleName ++ "." ++ tipe
