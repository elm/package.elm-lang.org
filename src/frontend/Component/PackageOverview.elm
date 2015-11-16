module Component.PackageOverview where

import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Regex
import String
import Task

import Docs.Package as Docs
import Docs.Entry as Entry
import Page.Context as Ctx
import Utils.Markdown as Markdown


type alias Model =
  { context : Ctx.OverviewContext
  }



-- INIT


init : Ctx.OverviewContext -> (Model, Effects Action)
init context =
  ( Model context
  , Fx.none
  )



-- UPDATE


type Action
    = NoOp


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
        ( model
        , Fx.none
        )



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view addr model =
  div [ class "pkg-overview" ]
    [ text "Coming soon: an overview of all releases of this library."
    ]
