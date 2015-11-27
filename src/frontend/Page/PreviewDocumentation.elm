module Page.PreviewDocumentation where

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict
import Json.Decode as Json exposing ((:=))
--import Html.Attributes exposing (..)

import Route
import Component.Header as Header
import Component.PackageDocs as PDocs
--import Component.PackageSidebar as PkgNav
import Docs.Package as Docs



-- PORTS

port fileReader : Signal { fileText : String }



-- SIGNALS

main : Signal Html
main =
  Signal.map (view actionsInbox.address) models


dummySignal : Signal.Mailbox PDocs.Action
dummySignal =
  Signal.mailbox PDocs.NoOp


actionsInbox : Signal.Mailbox Action
actionsInbox =
  Signal.mailbox NoOp


actions : Signal Action
actions =
  Signal.merge actionsInbox.signal loadedJsons


loadedJsons : Signal Action
loadedJsons =
  Signal.map (LoadDocs << .fileText) fileReader


models : Signal Model
models =
  Signal.foldp update initialModel actions


-- MODEL

type alias Model =
  { header : Header.Model
  , currentModuleDoc : PDocs.Model
  , moduleDocs : Dict.Dict String Docs.Module
  }


initialModel : Model
initialModel =
  { header = Header.Model Route.Tools
  , currentModuleDoc = PDocs.Loading
  , moduleDocs = Dict.empty
  }



-- UPDATE

type Action
  = NoOp
  | LoadDocs String


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    LoadDocs fileText ->
      let
        docs = loadDocs fileText
      in
        { model
          | currentModuleDoc = rawDocs (firstModuleName docs) docs
          , moduleDocs = docs
        }



-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  Header.view dummySignal.address model.header
    [ node "script" [ src "/assets/js/jsonLoader.js" ] []
    , div []
      [ h1 [] [ text "Preview your documentation" ]
      , input [ type' "file", id "fileLoader" ] []
      , hr [] []
      ]
    , PDocs.view dummySignal.address model.currentModuleDoc
    ]
  --Element.show rawDocs
  --PDocs.view dummySignal.address rawDocs


-- FUNCTIONS

loadDocs : String -> Dict.Dict String Docs.Module
loadDocs fileText =
  getModules fileText


firstModuleName : Dict.Dict String Docs.Module -> String
firstModuleName modules =
  Dict.keys modules
    |> List.head
    |> Maybe.withDefault ""


getModules : String -> Dict.Dict String Docs.Module
getModules docs =
  Json.decodeString Docs.decodePackage docs
  |> Result.withDefault Dict.empty


rawDocs : String -> Dict.Dict String Docs.Module -> PDocs.Model
rawDocs moduleName docs =
  case Dict.get moduleName docs of
    Just moduleDocs ->
      let
        chunks =
          PDocs.toChunks moduleDocs
      in
        PDocs.RawDocs (PDocs.Info moduleName (PDocs.toNameDict docs) chunks)

    Nothing ->
      PDocs.Loading



-- MOCKS

readme : PDocs.Model
readme = PDocs.Readme ("# Elm Collision\n\nDetect collision/intersection of geometry in a defined coordinate space, AKA: tell me when objects are touching or overlapping\n\n![elm-collision demo](https://raw.githubusercontent.com/burabure/elm-collision/master/elm-collision.gif)\n\nThis library is useful for games, interactive apps, dynamic element composition and other cases where you need very efficient detection of overlapping objects\n\n\n### Get Started\n\n- Read the [the documentation][docs].\n- Try and read the code of [the examples][examples].\n\n[docs]: http://package.elm-lang.org/packages/BuraBure/elm-collision/latest/\n[examp…:\nhttp://github.com/burabure/elm-collision/tree/master/examples/\n\n\n### Contributing\n\nDo you have a suggestion, algorithm or formula that you'd like to add to this library?, I'd love to take a look at it and help you get it working with the library, just post an issue or send a pull request =D\n")
