module Component.Catalog where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (..)
import Http
import String
import Task

import Docs.Summary as Summary
import Docs.Version as Vsn
import Utils.FluidList as FluidList
import Utils.Markdown as Markdown



-- MODEL


type Model
    = Loading
    | Failed Http.Error
    | Success (List Summary.Summary)



-- INIT


init : (Model, Effects Action)
init =
  ( Loading
  , getSummaries
  )



-- UPDATE


type Action
    = Fail Http.Error
    | Load (List Summary.Summary)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Fail httpError ->
        ( Failed httpError
        , Fx.none
        )

    Load summaries ->
        ( Success summaries
        , Fx.none
        )



-- EFFECTS


getSummaries : Effects Action
getSummaries =
  Http.get Summary.decoder "/all-packages"
    |> Task.map Load
    |> flip Task.onError (Task.succeed << Fail)
    |> Fx.task



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view addr model =
  div [class "catalog"] <|
    case model of
      Loading ->
          [ p [] [text "Loading..."]
          ]

      Failed httpError ->
          [ p [] [text "Problem loading package list!"]
          , p [] [text (toString httpError)]
          ]

      Success summaries ->
          [ FluidList.fluidList "pkg-summary" 300 3 (List.map viewSummary summaries)
          ]


viewSummary : Summary.Summary -> List Html
viewSummary summary =
  case List.maximum summary.versions of
    Just maxVersion ->
      let
        url =
          "/packages/" ++ summary.name ++ "/" ++ Vsn.vsnToString maxVersion
      in
        [ h1 [] [ a [ href url ] [ text summary.name ] ]
        , p [] [ text summary.summary ]
        ]

    Nothing ->
        [ h1 [] [ text summary.name ]
        , p [] [ text summary.summary ]
        ]

