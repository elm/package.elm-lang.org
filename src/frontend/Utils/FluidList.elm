module Utils.FluidList where

import Html exposing (..)
import Html.Attributes exposing (..)


(=>) = (,)


fluidList : String -> Int -> Int -> List (List Html) -> Html
fluidList itemClass itemWidth maxColumns itemList =
  let
    toPx : Int -> String
    toPx num =
      toString num ++ "px"

    bulletStyle =
        [ "display" => "inline-block"
        , "width" => toPx itemWidth
        , "vertical-align" => "top"
        , "text-align" => "left"
        , "margin" => ("0 " ++ toPx gutter)
        ]

    gutter = 30
  in
    section
      [style ["max-width" => toPx (itemWidth*maxColumns + 2*gutter*maxColumns), "margin" => "auto", "text-align" => "center", "margin-top" => "30px"]]
      (List.map (section [class itemClass, style bulletStyle]) itemList)
