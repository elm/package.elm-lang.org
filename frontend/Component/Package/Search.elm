module Component.Package.Search (view) where

import Graphics.Element exposing (Element)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
import Markdown


view : Signal.Address String -> Int -> Bool -> String -> Element
view fieldAddr width isCore fieldContent =
  toElement width 150 <|
  div []
    [ input
        [ placeholder "Package Search"
        , value fieldContent
        , on "input" targetValue (Signal.message fieldAddr)
        , inputStyle
        ]
        []
    , lazy description isCore
    ]


description : Bool -> Html
description isCore =
  let hint =
        if isCore
          then " Try searching for `|>` or `map`."
          else ""
  in
      Markdown.toHtml <|
        "<span style=\"font-size: 12px; color: rgb(216, 221, 225);\">"
        ++ "Search through all the functions and operators in this package."
        ++ hint
        ++ "</span>"


inputStyle =
  style
    [ ("display", "block")
    , ("margin", "10px 0 0 0")
    , ("padding", "10px")
    , ("border-style", "solid")
    , ("border-width", "1px")
    , ("border-color", "rgb(216, 221, 225)")
    , ("width", "178px")
    , ("appearance", "none")
    , ("box-shadow", "none")
    , ("border-radius", "4px")
    ]
