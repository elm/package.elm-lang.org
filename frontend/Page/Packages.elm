module Page.Packages where

import Color
import ColorScheme as C
import Graphics.Element (..)
import Http
import Json.Decode as Json
import Signal
import String
import Window

import Component.TopBar as TopBar
import Component.Packages as Packages


port title : String
port title =
    "Elm Packages"


main : Signal Element
main =
    Signal.map2 view Window.dimensions packages


search : Signal.Channel TopBar.Update
search =
    Signal.channel TopBar.NoOp


view : (Int,Int) -> List Packages.Package -> Element
view (windowWidth, windowHeight) packages =
  color C.background <|
  flow down
  [ TopBar.view windowWidth search (TopBar.Model TopBar.Global "map" TopBar.Normal)
  , flow right
    [ spacer ((windowWidth - 980) // 2) (windowHeight - TopBar.topBarHeight)
    , Packages.view 980 packages
    ]
  ]


allPackagesUrl : String
allPackagesUrl =
    "/all-packages"


packages : Signal (List Packages.Package)
packages =
    Http.sendGet (Signal.constant allPackagesUrl)
      |> Signal.map handleResult


handleResult : Http.Response String -> List Packages.Package
handleResult response =
  case response of
    Http.Success msg ->
      case Json.decodeString (Json.list Packages.package) msg of
        Ok packages -> packages
        Err _ -> []

    _ -> []
