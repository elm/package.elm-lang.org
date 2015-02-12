module Component.Package.Search (view) where

import Graphics.Element (Element)
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import LocalChannel as LC
import Markdown

import Component.Package.ModuleList as ModuleList

view : LC.LocalChannel String -> Int -> ModuleList.Model -> String -> Element
view fieldChan width pkg fieldContent =
  toElement width 150 <|
  div []
    [ input
        [ placeholder "Package Search"
        , value fieldContent
        , on "input" targetValue (LC.send fieldChan)
        , inputStyle
        ]
        []
    , description pkg
    ]


description : ModuleList.Model -> Html
description pkg = Markdown.toHtml <| """

<span style="font-size: 12px; color: rgb(216, 221, 225);">
Search through all the functions and operators in this package.
"""
    ++ (if isCore pkg then " Try searching for `|>` or `map`." else "") ++ "</span>"


isCore : ModuleList.Model -> Bool
isCore pkg =
    pkg.user == "elm-lang" && pkg.name == "core"


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
