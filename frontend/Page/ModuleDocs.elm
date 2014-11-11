module Page.ModuleDocs where

import Color
import ColorScheme as C
import Dict
import Json.Decode as Json
import Graphics.Element (..)
import Http
import List
import Signal
import String
import Window

import Component.TopBar as TopBar
import Component.ModuleDocs as Docs
import Component.Documentation as D


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


documentation : Signal D.Documentation
documentation =
    Http.sendGet (Signal.constant documentationUrl)
      |> Signal.map handleResult


dummyDocs : D.Documentation
dummyDocs =
  D.Documentation context.moduleName "Loading documentation..." [] [] []


handleResult : Http.Response String -> D.Documentation
handleResult response =
  case response of
    Http.Success string ->
      case Json.decode D.documentation string of
        Ok docs -> docs
        Err msg ->
            { dummyDocs |
                comment <- "There was an error loading these docs! They may be corrupted."
            }

    _ -> dummyDocs


main : Signal Element
main =
    Signal.map2 view Window.dimensions documentation


search : Signal.Channel TopBar.Update
search =
    Signal.channel TopBar.NoOp


view : (Int,Int) -> D.Documentation -> Element
view (windowWidth, windowHeight) docs =
  color C.background <|
  flow down
  [ TopBar.view windowWidth search (TopBar.Model TopBar.Global "map" TopBar.Normal)
  , flow right
    [ spacer ((windowWidth - 980) // 2) (windowHeight - TopBar.topBarHeight)
    , Docs.view 980 "elm-lang" "core" docs
    ]
  ]
