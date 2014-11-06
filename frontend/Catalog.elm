module Catalog where

import Basics (..)
import Color
import ColorScheme as C
import Json
import Graphics.Element (..)
import List
import Signal
import String
import Text
import Window

import TopBar


main : Signal.Signal Element
main =
    Signal.map scene Window.dimensions


search : Signal.Channel TopBar.Update
search =
    Signal.channel TopBar.NoOp


scene : (Int,Int) -> Element
scene (windowWidth, windowHeight) =
  color C.background <|
  flow down
  [ TopBar.view windowWidth search (TopBar.Model TopBar.Global "map" TopBar.Hover)
  , spacer windowWidth windowHeight
  ]
