module Page.PackageDocs where

import Basics (..)
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
import Component.PackageDocs as Package
import Component.Package.ModuleList as ModuleList
import Component.Documentation as Doc


port context : { user : String, name : String, version : String, versionList : List String }

port title : String
port title =
    context.user ++ "/" ++ context.name ++ " " ++ context.version


packageUrl : String -> String
packageUrl version =
  "/packages/" ++ context.user ++ "/" ++ context.name ++ "/" ++ version


documentationUrl : String
documentationUrl =
  packageUrl context.version ++ "/documentation.json"


moduleList : Signal ModuleList.Model
moduleList =
    Http.sendGet (Signal.constant documentationUrl)
      |> Signal.map handleResult


packageInfo : List (String, List String) -> ModuleList.Model
packageInfo modules =
  ModuleList.Model context.user context.name context.version context.versionList modules


handleResult : Http.Response String -> ModuleList.Model
handleResult response =
  case response of
    Http.Success msg ->
      case Json.decodeString (Json.list Doc.valueList) msg of
        Err _ -> packageInfo []
        Ok modules ->
            packageInfo modules

    _ -> packageInfo []


readmeUrl : String
readmeUrl =
  packageUrl context.version ++ "/README.md"


readme : Signal (Maybe String)
readme =
    Http.sendGet (Signal.constant readmeUrl)
      |> Signal.map extractReadme


extractReadme : Http.Response String -> Maybe String
extractReadme response =
  case response of
    Http.Success str -> Just str
    _ -> Nothing


main : Signal Element
main =
    Signal.map4 view Window.dimensions moduleList (Signal.subscribe searchChan) readme


searchChan : Signal.Channel String
searchChan =
    Signal.channel ""


versionChan : Signal.Channel String
versionChan =
    Signal.channel ""


port redirect : Signal String
port redirect =
  Signal.keepIf ((/=) "") "" (Signal.subscribe versionChan)
    |> Signal.map packageUrl


view : (Int,Int) -> ModuleList.Model -> String -> Maybe String -> Element
view (windowWidth, windowHeight) moduleList searchTerm readme =
  color C.background <|
  flow down
  [ TopBar.view windowWidth
  , flow right
    [ spacer ((windowWidth - 980) // 2) (windowHeight - TopBar.topBarHeight)
    , Package.view
        (LC.create identity versionChan)
        (LC.create identity searchChan)
        980
        moduleList
        searchTerm
        readme
    ]
  ]
