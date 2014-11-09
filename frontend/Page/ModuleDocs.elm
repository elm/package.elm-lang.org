module Page.ModuleDocs where

import Basics (..)
import Color
import ColorScheme as C
import Debug
import Dict
import JavaScript as JS
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
import Component.ModuleDocs as Docs


port context : { user : String, name : String, version : String, moduleName : String }

port title : String
port title =
    context.user ++ "/" ++ context.name ++ " " ++ context.moduleName ++ " " ++ context.version


documentationUrl : String
documentationUrl =
  let name = String.map (\c -> if c == '.' then '-' else c) context.moduleName
  in
      "/packages/" ++ context.user ++ "/" ++ context.name ++ "/"
      ++ context.version ++ "/docs/" ++ name ++ ".json"


documentation : Signal.Signal Docs.Documentation
documentation =
    Http.sendGet (Signal.constant documentationUrl)
      |> Signal.map handleResult


dummyDocs : Docs.Documentation
dummyDocs =
  Docs.Documentation context.moduleName "Loading documentation..." [] [] []


handleResult : Http.Response String -> Docs.Documentation
handleResult response =
  case response of
    Http.Success string ->
      case Result.andThen (JS.fromString string) (JS.get Docs.documentation) of
        Result.Ok docs -> docs
        Result.Err msg -> { dummyDocs | comment <- Debug.log "error" msg }

    _ -> dummyDocs


main : Signal.Signal Element
main =
    Signal.map2 view Window.dimensions documentation


search : Signal.Channel TopBar.Update
search =
    Signal.channel TopBar.NoOp


view : (Int,Int) -> Docs.Documentation -> Element
view (windowWidth, windowHeight) docs =
  color C.background <|
  flow down
  [ TopBar.view windowWidth search (TopBar.Model TopBar.Global "map" TopBar.Normal)
  , flow right
    [ spacer ((windowWidth - 980) // 2) (windowHeight - TopBar.topBarHeight)
    , Docs.view 980 "elm-lang" "core" docs
    ]
  ]
