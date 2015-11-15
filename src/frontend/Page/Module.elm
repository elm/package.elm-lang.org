module Page.Module where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp
import Task

import Component.Header as Header
import Component.ModuleDocs as MDocs
import Component.PackageNavigator as PkgNav
import Page.Context as Ctx
import Route



-- WIRES


port context : Ctx.Context


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
    , moduleDocs : MDocs.Model
    , pkgNav : PkgNav.Model
    }



-- INIT


init : (Model, Effects Action)
init =
  let
    (header, headerFx) =
      Header.init (Route.fromContext context)

    (moduleDocs, moduleFx) =
      MDocs.init context

    (pkgNav, navFx) =
      PkgNav.init context
  in
    ( Model header moduleDocs pkgNav
    , Fx.batch
        [ headerFx
        , Fx.map UpdateDocs moduleFx
        , Fx.map UpdateNav navFx
        ]
    )



-- UPDATE


type Action
    = UpdateDocs MDocs.Action
    | UpdateNav PkgNav.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdateDocs act ->
        let
          (newDocs, fx) =
            MDocs.update act model.moduleDocs
        in
          ( { model | moduleDocs = newDocs }
          , Fx.map UpdateDocs fx
          )

    UpdateNav act ->
        let
          (newPkgNav, fx) =
            PkgNav.update act model.pkgNav
        in
          ( { model | pkgNav = newPkgNav }
          , Fx.map UpdateNav fx
          )



-- VIEW


view : Signal.Address Action -> Model -> Html
view addr model =
  div []
    [ Header.view addr model.header
    , div [class "center"]
        [ MDocs.view (Signal.forwardTo addr UpdateDocs) model.moduleDocs
        , PkgNav.view (Signal.forwardTo addr UpdateNav) model.pkgNav
        ]
    ]





