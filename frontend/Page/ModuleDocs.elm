module Page.ModuleDocs where

import Color
import ColorScheme as C
import Dict
import Json.Decode as Json
import Graphics.Element (..)
import Http
import List
import LocalChannel as LC
import Signal
import String
import Window

import Component.TopBar as TopBar
import Component.ModuleDocs as Docs
import Component.Documentation as D


port context : { user : String, name : String, version : String, versionList : List String, moduleName : String }

port title : String
port title =
    context.user ++ "/" ++ context.name ++ " " ++ context.version ++ " " ++ context.moduleName


packageUrl : String -> String
packageUrl version =
  "/packages/" ++ context.user ++ "/" ++ context.name ++ "/" ++ version


moduleNameToUrl : String -> String
moduleNameToUrl name =
  String.map (\c -> if c == '.' then '-' else c) name


documentationUrl : String
documentationUrl =
  let name = moduleNameToUrl context.moduleName
  in
      packageUrl context.version ++ "/docs/" ++ name ++ ".json"


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
      case Json.decodeString D.documentation string of
        Ok docs -> docs
        Err msg ->
            { dummyDocs |
                comment <- "There was an error loading these docs! They may be corrupted."
            }

    _ -> dummyDocs


main : Signal Element
main =
    Signal.map2 view Window.dimensions documentation


versionChan : Signal.Channel String
versionChan =
    Signal.channel ""


port redirect : Signal String
port redirect =
  Signal.keepIf ((/=) "") "" (Signal.subscribe versionChan)
    |> Signal.map (\v -> packageUrl v ++ "/" ++ moduleNameToUrl context.moduleName)


view : (Int,Int) -> D.Documentation -> Element
view (windowWidth, windowHeight) docs =
  color C.background <|
  flow down
  [ TopBar.view windowWidth
  , flow right
    [ spacer ((windowWidth - 980) // 2) (windowHeight - TopBar.topBarHeight)
    , Docs.view (LC.create identity versionChan) 980 context.user context.name context.version context.versionList docs
    ]
  ]
