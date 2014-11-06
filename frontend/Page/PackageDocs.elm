module Page.PackageDocs where

import Basics (..)
import Color
import ColorScheme as C
import Json
import Graphics.Element (..)
import Signal
import String
import Window

import Component.TopBar as TopBar
import Component.PackageDocs as Docs


main : Signal.Signal Element
main =
    Signal.map2 scene Window.dimensions (Signal.constant Docs.dummyModel)


search : Signal.Channel TopBar.Update
search =
    Signal.channel TopBar.NoOp


scene : (Int,Int) -> Docs.Model -> Element
scene (windowWidth, windowHeight) packages =
  let packageDocs =
        Docs.view 980 packages
  in
  color C.background <|
  flow down
  [ TopBar.view windowWidth search (TopBar.Model TopBar.Global "map" TopBar.Normal)
  , container windowWidth (max windowHeight (heightOf packageDocs)) midTop packageDocs
  ]
