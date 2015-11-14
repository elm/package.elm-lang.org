module Page.PackageList where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp
import Task

import Component.ModuleDocs as MDocs
import Component.PackageNavigator as PkgNav
import Header
import Page.Context as Ctx
import Route


-- WIRES

port title : String
port title =
    "Elm Packages"


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
    context =
      Ctx.Context "elm-lang" "core" "3.0.0" ["3.0.0","2.0.0","1.0.0"] (Just "Graphics.Element")

    (header, headerFx) =
      Header.init Route.dummy

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





