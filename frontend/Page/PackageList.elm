module Page.PackageList where

import Color
import ColorScheme as C
import Graphics.Element exposing (..)
import Http
import Json.Decode as Json
import String
import Task exposing (Task, andThen, onError)
import Window

import Component.TopBar as TopBar
import Component.PackageList as PackageList


port title : String
port title =
    "Elm Packages"


main : Signal Element
main =
    Signal.map2 view Window.dimensions packages.signal


view : (Int,Int) -> List PackageList.Package -> Element
view (windowWidth, windowHeight) packages =
  let innerWidth = min 980 windowWidth
  in
      color C.background <|
      flow down
      [ TopBar.view windowWidth
      , flow right
        [ spacer ((windowWidth - innerWidth) // 2) (windowHeight - TopBar.topBarHeight)
        , PackageList.view innerWidth packages
        ]
      ]


allPackagesUrl : String
allPackagesUrl =
    "/all-packages"


packages : Signal.Mailbox (List PackageList.Package)
packages =
  Signal.mailbox []


port getPackages : Task x ()
port getPackages =
  let
    get =
      Http.get (Json.list PackageList.package) allPackagesUrl

    recover _ =
      Task.succeed []
  in
    (get `onError` recover) `andThen` Signal.send packages.address
