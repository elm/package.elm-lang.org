module Page.Package exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)

import Component.Header as Header
import Component.PackageDocs as PDocs
import Component.PackageSidebar as PkgNav
import Component.Description as Desc
import Page.Context as Ctx
import Route



-- WIRES

main : Program Ctx.VersionContext
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
    , desc : Desc.Model
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

    (desc, descCmd) =
      Desc.init context
  in
    ( Model header moduleDocs pkgNav desc
    , Cmd.batch
        [ headerCmd
        , Cmd.map UpdateDocs moduleCmd
        , Cmd.map UpdateNav navCmd
        , Cmd.map UpdateDesc descCmd
        ]
    )



-- UPDATE


type Msg
    = UpdateDocs PDocs.Msg
    | UpdateNav PkgNav.Msg
    | UpdateDesc Desc.Msg


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

    UpdateDesc descMsg ->
        let
          (newDesc, fx) =
            Desc.update descMsg model.desc
        in
          ( { model | desc = newDesc }
          , Cmd.map UpdateDesc fx
          )



-- VIEW


view : Model -> Html Msg
view model =
  Header.view model.header
    [ Html.map UpdateDocs (PDocs.view model.moduleDocs)
    , div
        [ class "pkg-nav" ]
        [ Html.map UpdateNav (PkgNav.view model.pkgNav)
        , Html.map UpdateDesc (Desc.view model.desc)
        ]
    ]


