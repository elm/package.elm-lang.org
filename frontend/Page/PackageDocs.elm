module Page.PackageDocs where

import Basics (..)
import Color
import ColorScheme as C
import Dict
import Json
import Graphics.Element (..)
import Http
import List
import List ((++))
import Maybe (..)
import Result
import Signal
import String
import Window

import Component.TopBar as TopBar
import Component.PackageDocs as Docs


port context : { user : String, name : String, version : String }

port title : String
port title =
    context.user ++ "/" ++ context.name ++ " - " ++ context.version


descriptionUrl : String
descriptionUrl =
  "/description?name=" ++ context.user ++ "/" ++ context.name ++ "&version=" ++ context.version


description : Signal.Signal Docs.PackageInfo
description =
    Http.sendGet (Signal.constant descriptionUrl)
      |> Signal.map handleResult


packageInfo : [String] -> Docs.PackageInfo
packageInfo modules =
  Docs.PackageInfo context.user context.name context.version modules


handleResult : Http.Response String -> Docs.PackageInfo
handleResult response =
  case response of
    Http.Success string ->
      case Json.fromString string of
        Result.Ok (Json.Object dict) ->
          case Dict.get "exposed-modules" dict of
            Just (Json.Array modules) ->
                packageInfo (List.map (\(Json.String s) -> s) modules)
            _ -> packageInfo []

        _ -> packageInfo []

    _ -> packageInfo []


readme : Signal.Signal (Maybe String)
readme =
    Signal.constant Nothing


main : Signal.Signal Element
main =
    Signal.map3 view Window.dimensions description readme


search : Signal.Channel TopBar.Update
search =
    Signal.channel TopBar.NoOp


view : (Int,Int) -> Docs.PackageInfo -> Maybe String -> Element
view (windowWidth, windowHeight) packages readme =
  color C.background <|
  flow down
  [ TopBar.view windowWidth search (TopBar.Model TopBar.Global "map" TopBar.Normal)
  , flow right
    [ spacer ((windowWidth - 980) // 2) (windowHeight - TopBar.topBarHeight)
    , Docs.view 980 packages readme
    ]
  ]
