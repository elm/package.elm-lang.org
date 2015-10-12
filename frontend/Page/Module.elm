module Page.Module where

import Color
import ColorScheme as C
import Dict
import Json.Decode as Json
import Graphics.Element exposing (..)
import Http
import String
import Task exposing (Task, andThen, onError, succeed)
import Window
import Set

import Component.TopBar as TopBar
import Component.Module as Module
import Component.Documentation as D


port context : { user : String, name : String, version : String, versionList : List String, moduleName : String }

port title : String
port title =
  context.moduleName ++ " - " ++ context.name ++ " " ++ context.version


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


port getDocs : Task x ()
port getDocs =
  let
    get = Http.get D.documentation documentationUrl

    recover _ =
        succeed (dummyDocs "There was an error loading these docs! They may be corrupted.")
  in
    (get `onError` recover)
        `andThen` Signal.send documentation.address


documentation : Signal.Mailbox D.Documentation
documentation =
  Signal.mailbox (dummyDocs "Loading documentation...")


dummyDocs : String -> D.Documentation
dummyDocs msg =
  D.Documentation context.moduleName msg [] [] []


main : Signal Element
main =
    Signal.map3 view Window.dimensions modulesAndMentionedTypes.signal documentation.signal


version : Signal.Mailbox String
version =
    Signal.mailbox ""


port redirect : Signal String
port redirect =
  Signal.filter ((/=) "") "" version.signal
    |> Signal.map (\v -> packageUrl v ++ "/" ++ moduleNameToUrl context.moduleName)


port docsLoaded : Signal ()
port docsLoaded =
  Signal.map (always ()) documentation.signal


port getModulesAndMentionedTypes : Task x ()
port getModulesAndMentionedTypes =
  let
    get =
      Http.get (Json.list D.documentation) (packageUrl context.version ++ "/documentation.json")

    recover _ =
      Task.succeed []

    removeDuplicates =
      Set.toList << Set.fromList

    send list =
      Signal.send modulesAndMentionedTypes.address (List.map .name list, removeDuplicates (List.concatMap D.mentionedTypes list))
  in
    (get `onError` recover) `andThen` send


modulesAndMentionedTypes : Signal.Mailbox (List String, List (String, String))
modulesAndMentionedTypes =
  Signal.mailbox ([], [])


view : (Int,Int) -> (List String, List (String, String)) -> D.Documentation -> Element
view (windowWidth, windowHeight) (modules, mentionedTypes) docs =
  let innerWidth = min 980 windowWidth
  in
    color C.background <|
    flow down
    [ TopBar.view windowWidth
    , flow right
      [ spacer ((windowWidth - innerWidth) // 2) (windowHeight - TopBar.topBarHeight)
      , Module.view version.address innerWidth context.user context.name context.version context.versionList modules mentionedTypes docs
      ]
    ]
