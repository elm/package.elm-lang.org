module Page.PackageList where

import Color
import ColorScheme as C
import Graphics.Element exposing (..)
import Http
import Json.Decode as Json
import Set
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
    Signal.map3 view Window.dimensions newList.signal packages.signal


view : (Int,Int) -> List String -> List PackageList.Package -> Element
view (windowWidth, windowHeight) newList packages =
  let innerWidth = min 980 windowWidth
  in
      color C.background <|
      flow down
      [ TopBar.view windowWidth
      , flow right
        [ spacer ((windowWidth - innerWidth) // 2) (windowHeight - TopBar.topBarHeight)
        , PackageList.view innerWidth newList packages
        ]
      ]


-- GET ALL PACKAGES

packages : Signal.Mailbox (List PackageList.Package)
packages =
  Signal.mailbox []


port getPackages : Task x ()
port getPackages =
  let
    get =
      Http.get (Json.list PackageList.package) "/all-packages"

    recover _ =
      Task.succeed []
  in
    (get `onError` recover) `andThen` Signal.send packages.address


-- GET ALL NEW PACKAGES

newList : Signal.Mailbox (List String)
newList =
  Signal.mailbox []


port getNewPackages : Task x ()
port getNewPackages =
  let
    get =
      Http.get (Json.list Json.string) "/new-packages"

    recover _ =
      Task.succeed []
  in
    (get `onError` recover) `andThen` Signal.send newList.address
