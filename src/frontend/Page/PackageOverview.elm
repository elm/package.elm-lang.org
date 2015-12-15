module Page.PackageOverview where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp
import Task

import Component.Header as Header
import Overview.Component.Changes as Changes
import Overview.Component.Links as Links
import Overview.History as History
import Page.Context as Ctx
import Route



-- WIRES


port context : Ctx.OverviewContext


port rawHistory : List History.RawRelease


history : History.History
history =
  List.map History.processRaw rawHistory


app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = []
    }


main =
  app.html


port worker : Signal (Task.Task Fx.Never ())
port worker =
  app.tasks



-- MODEL


type alias Model =
    { header : Header.Model
    , links : Links.Model
    , changes : Changes.Model
    }



-- INIT


init : (Model, Effects Action)
init =
  let
    (header, headerFx) =
      Header.init (Route.fromOverviewContext context)

    (links, linksFx) =
      Links.init context

    (changes, changesFx) =
      Changes.init context history
  in
    ( Model header links changes
    , Fx.batch
        [ headerFx
        , Fx.map UpdateLinks linksFx
        , Fx.map UpdateChanges changesFx
        ]
    )



-- UPDATE


type Action
    = UpdateLinks Links.Action
    | UpdateChanges Changes.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdateLinks act ->
        let
          (newDocs, fx) =
            Links.update act model.links
        in
          ( { model | links = newDocs }
          , Fx.map UpdateLinks fx
          )

    UpdateChanges act ->
        let
          (newChanges, fx) =
            Changes.update context act model.changes
        in
          ( { model | changes = newChanges }
          , Fx.map UpdateChanges fx
          )



-- VIEW


view : Signal.Address Action -> Model -> Html
view addr model =
  Header.view addr model.header
    [ Links.view (Signal.forwardTo addr UpdateLinks) model.links
    , Changes.view (Signal.forwardTo addr UpdateChanges) model.changes
    ]


