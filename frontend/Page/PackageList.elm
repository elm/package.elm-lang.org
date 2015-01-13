module Page.PackageList where

import Color
import ColorScheme as C
import Graphics.Element (..)
import Http
import Json.Decode as Json
import Signal
import String
import Window

import Component.TopBar as TopBar
import Component.PackageList as PackageList


port title : String
port title =
    "Elm Packages"


main : Signal Element
main =
    Signal.map2 view Window.dimensions packages


view : (Int,Int) -> List PackageList.Package -> Element
view (windowWidth, windowHeight) packages =
  color C.background <|
  flow down
  [ TopBar.view windowWidth
  , flow right
    [ spacer ((windowWidth - 980) // 2) (windowHeight - TopBar.topBarHeight)
    , PackageList.view 980 packages
    ]
  ]


allPackagesUrl : String
allPackagesUrl =
    "/all-packages"


packages : Signal (List PackageList.Package)
packages =
    Http.sendGet (Signal.constant allPackagesUrl)
      |> Signal.map handleResult


handleResult : Http.Response String -> List PackageList.Package
handleResult response =
  case response of
    Http.Success msg ->
      case Json.decodeString (Json.list PackageList.package) msg of
        Ok packages -> packages
        Err _ -> []

    _ -> []
