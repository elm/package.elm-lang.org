module Utils.Code exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)



keyword : String -> Html msg
keyword kw =
  span [class "hljs-keyword"] [text kw]


addParens : List (Html msg) -> List (Html msg)
addParens list =
  text "(" :: list ++ [text ")"]


space : Html msg
space =
  text " "


padded : Html msg -> List (Html msg)
padded html =
  [ space, html, space ]


arrow : Html msg
arrow =
  span [] [text "->"]


colon : Html msg
colon =
  span [] [text ":"]


equals : Html msg
equals =
  span [] [text "="]


