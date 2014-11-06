module Page.ModuleDocs where

import Basics (..)
import Color
import ColorScheme as C
import Json
import Graphics.Element (..)
import Signal
import String
import Window

import Component.TopBar as TopBar
import Component.ModuleDocs as Docs


main : Signal.Signal Element
main =
    Signal.map2 scene Window.dimensions (Signal.constant Docs.dummyDocs)


search : Signal.Channel TopBar.Update
search =
    Signal.channel TopBar.NoOp


scene : (Int,Int) -> Docs.Documentation -> Element
scene (windowWidth, windowHeight) docs =
  let packageDocs =
        Docs.view 980 "elm-lang" "core" docs
  in
  color C.background <|
  flow down
  [ TopBar.view windowWidth search (TopBar.Model TopBar.Global "map" TopBar.Normal)
  , container windowWidth (max windowHeight (heightOf packageDocs)) midTop packageDocs
  ]
