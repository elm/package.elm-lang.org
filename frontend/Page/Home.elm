module Page.Home where

import Basics (..)
import Color
import ColorScheme as C
import Json
import Graphics.Element (..)
import Signal
import String
import Window

import Component.TopBar as TopBar
import Component.Packages as Packages


main : Signal.Signal Element
main =
    Signal.map2 scene Window.dimensions (Signal.constant [Packages.Package "elm-lang/core" "core libraries" ["1.0.0"]])


search : Signal.Channel TopBar.Update
search =
    Signal.channel TopBar.NoOp


scene : (Int,Int) -> [Packages.Package] -> Element
scene (windowWidth, windowHeight) packages =
  let packageListing =
        Packages.view 980 packages
  in
  color C.background <|
  flow down
  [ TopBar.view windowWidth search (TopBar.Model TopBar.Global "map" TopBar.Normal)
  , container windowWidth (max windowHeight (heightOf packageListing)) midTop packageListing
  ]
