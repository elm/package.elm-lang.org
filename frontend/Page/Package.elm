module Page.Package where

import Color
import ColorScheme as C
import Dict
import Json.Decode as Json
import Graphics.Element exposing (..)
import Http
import String
import Task exposing (Task, andThen, onError)
import Window

import Component.TopBar as TopBar
import Component.Package as Package
import Component.Package.ModuleList as ModuleList
import Component.Documentation as Doc


type alias Context =
    { user : String
    , name : String
    , version : String
    , versionList : List String
    }


port context : Context


port title : String
port title =
    context.user ++ "/" ++ context.name ++ " " ++ context.version


packageUrl : String -> String
packageUrl version =
  "/packages/" ++ context.user ++ "/" ++ context.name ++ "/" ++ version


documentationUrl : String
documentationUrl =
  packageUrl context.version ++ "/documentation.json"


port getModuleList : Task x ()
port getModuleList =
  let
    get =
      Http.get (Json.list Doc.valueList) documentationUrl

    recover _ =
      Task.succeed []

    send list =
      Signal.send moduleList.address (packageInfo (List.sort list))
  in
    (get `onError` recover) `andThen` send


moduleList : Signal.Mailbox ModuleList.Model
moduleList =
  Signal.mailbox (packageInfo [])


packageInfo : List (String, List (String, String)) -> ModuleList.Model
packageInfo modules =
  ModuleList.Model context.user context.name context.version context.versionList modules


readmeUrl : String
readmeUrl =
  packageUrl context.version ++ "/README.md"


readme : Signal.Mailbox (Maybe String)
readme =
  Signal.mailbox Nothing


port getReadme : Task x ()
port getReadme =
  Task.toMaybe (Http.getString readmeUrl)
    `andThen` Signal.send readme.address


main : Signal Element
main =
    Signal.map4 view Window.dimensions moduleList.signal searchMailbox.signal readme.signal


searchMailbox : Signal.Mailbox String
searchMailbox =
    Signal.mailbox ""


versionMailbox : Signal.Mailbox String
versionMailbox =
    Signal.mailbox ""


port redirect : Signal String
port redirect =
  Signal.filter ((/=) "") "" versionMailbox.signal
    |> Signal.map packageUrl


view : (Int,Int) -> ModuleList.Model -> String -> Maybe String -> Element
view (windowWidth, windowHeight) moduleList searchTerm readme =
  let innerWidth = min 980 windowWidth
  in
    color C.background <|
    flow down
    [ TopBar.view windowWidth
    , flow right
      [ spacer ((windowWidth - innerWidth) // 2) (windowHeight - TopBar.topBarHeight)
      , Package.view
          versionMailbox.address
          searchMailbox.address
          innerWidth
          moduleList
          searchTerm
          readme
      ]
    ]
