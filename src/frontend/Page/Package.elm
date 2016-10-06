module Page.Package exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Task

import Component.Header as Header
import Component.PackageDocs as PDocs
import Component.PackageSidebar as PkgNav
import Page.Context as Ctx
import Route



-- WIRES


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }



-- MODEL


type alias Model =
    { header : Header.Model
    , moduleDocs : PDocs.Model
    , pkgNav : PkgNav.Model
    }



-- INIT


init : Ctx.VersionContext -> (Model, Cmd Msg)
init context =
  let
    (header, headerCmd) =
      Header.init (Route.fromVersionContext context)

    (moduleDocs, moduleCmd) =
      PDocs.init context

    (pkgNav, navCmd) =
      PkgNav.init context
  in
    ( Model header moduleDocs pkgNav
    , Cmd.batch
        [ headerCmd
        , Cmd.map UpdateDocs moduleCmd
        , Cmd.map UpdateNav navCmd
        ]
    )



-- UPDATE


type Msg
    = UpdateDocs PDocs.Msg
    | UpdateNav PkgNav.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateDocs docsMsg ->
        let
          (newDocs, fx) =
            PDocs.update docsMsg model.moduleDocs
        in
          ( { model | moduleDocs = newDocs }
          , Cmd.map UpdateDocs fx
          )

    UpdateNav navMsg ->
        let
          (newPkgNav, fx) =
            PkgNav.update navMsg model.pkgNav
        in
          ( { model | pkgNav = newPkgNav }
          , Cmd.map UpdateNav fx
          )



-- VIEW


view : Model -> Html Msg
view model =
  Header.view model.header
    [ Html.map UpdateDocs (PDocs.view model.moduleDocs)
    , Html.map UpdateNav (PkgNav.view model.pkgNav)
    ]


