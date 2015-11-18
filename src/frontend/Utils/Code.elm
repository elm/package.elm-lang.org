module Utils.Code where

import Html exposing (..)
import Html.Attributes exposing (..)



keyword : String -> Html
keyword kw =
  span [class "hljs-keyword"] [text kw]


addParens : List Html -> List Html
addParens list =
  text "(" :: list ++ [text ")"]


space : Html
space =
  text " "


padded : Html -> List Html
padded html =
  [ space, html, space ]


arrow : Html
arrow =
  span [] [text "->"]


colon : Html
colon =
  span [] [text ":"]


equals : Html
equals =
  span [] [text "="]


