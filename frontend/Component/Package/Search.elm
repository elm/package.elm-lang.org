module Component.Package.Search (view) where

import Graphics.Element (Element)
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Html.Lazy (lazy)
import LocalChannel as LC
import Markdown


view : LC.LocalChannel String -> Int -> Bool -> String -> Element
view fieldChan width isCore fieldContent =
  toElement width 150 <|
  div []
    [ input
        [ placeholder "Package Search"
        , value fieldContent
        , on "input" targetValue (LC.send fieldChan)
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
