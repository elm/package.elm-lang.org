module Page.Versions where

import Basics (..)
import Color
import ColorScheme as C
import Dict
import Json.Decode (..)
import Json.Decode as Json
import Graphics.Element (..)
import Http
import List
import Signal
import String
import Text
import Window

import Component.TopBar as TopBar
import Component.PackageDocs as Docs


port context : { user : String, name : String }

port title : String
port title =
    context.user ++ "/" ++ context.name


versionsUrl : String
versionsUrl =
  "/versions?name=" ++ context.user ++ "/" ++ context.name


versions : Signal (List String)
versions =
    Signal.constant ["1.0.0", "1.0.1", "1.0.2", "1.1.0", "2.0.0"]


main : Signal Element
main =
    Signal.map2 view Window.dimensions versions


search : Signal.Channel TopBar.Update
search =
    Signal.channel TopBar.NoOp


view : (Int,Int) -> List String -> Element
view (windowWidth, windowHeight) versions =
  color C.background <|
  flow down
  [ TopBar.view windowWidth search (TopBar.Model TopBar.Global "map" TopBar.Normal)
  , flow right
    [ spacer ((windowWidth - 980) // 2) (windowHeight - TopBar.topBarHeight)
    , viewVersions 980 context.user context.name versions
    ]
  ]


viewVersions : Int -> String -> String -> List String -> Element
viewVersions innerWidth user package versions =
    let bigWords =
          Text.fromString (user ++ " / " ++ package)
            |> Text.height 24
            |> Text.leftAligned

        header =
          container innerWidth 100 midLeft bigWords
    in
    flow down
    [ header
    , color C.lightGrey (spacer innerWidth 1)
    , spacer innerWidth 12
    , Text.asText versions
    ]


