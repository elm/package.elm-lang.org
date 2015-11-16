module Component.Catalog where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, href, placeholder, style, value)
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
    | Success { summaries : List Summary.Summary, query : String }



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
    | Query String


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Fail httpError ->
        ( Failed httpError
        , Fx.none
        )

    Load summaries ->
        ( Success { summaries = summaries, query = "" }
        , Fx.none
        )

    Query query ->
      flip (,) Fx.none <|
        case model of
          Success facts ->
              Success { facts | query = query }

          Loading ->
              model

          Failed err ->
              model



searchFor : String -> List Summary.Summary -> List Summary.Summary
searchFor query summaries =
  let
    lowerQuery =
      String.toLower query

    contains {name,summary} =
      String.contains lowerQuery (String.toLower name)
      ||
      String.contains lowerQuery (String.toLower summary)
  in
    List.filter contains summaries



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

      Success {summaries, query} ->
          [ input
              [ placeholder "Search"
              , value query
              , on "input" targetValue (Signal.message addr << Query)
              , autofocus True
              ]
              []
          , div [] (List.map viewSummary (searchFor query summaries))
          ]


viewSummary : Summary.Summary -> Html
viewSummary summary =
  let
    url =
      "/packages/" ++ summary.name ++ "/latest"
  in
    div [class "pkg-summary"]
      [ div []
          [ h1 [] [ a [ href url ] [ text summary.name ] ]
          , helpfulLinks summary
          ]
      , p [class "pkg-summary-desc"] [ text summary.summary ]
      ]


helpfulLinks : Summary.Summary -> Html
helpfulLinks summary =
  let
    allInterestingVersions =
      Vsn.filterInteresting summary.versions

    len =
      List.length allInterestingVersions

    interestingVersions =
      if len > 3 then
          List.drop (len - 3) allInterestingVersions

      else
          allInterestingVersions

    starter =
      case interestingVersions of
        (1,0,0) :: _ ->
          []

        _ ->
          [ text "…" ]
  in
    span [ class "pkg-summary-hints" ] <| List.intersperse (text " ") <|
      starter
      ++ List.intersperse (text "…") (List.map (versionLink summary.name) interestingVersions)
      ++  [ text "—"
          , a [ href ("/packages/" ++ summary.name) ] [ text "Overview" ]
          ]


versionLink : String -> Vsn.Version -> Html
versionLink packageName vsn =
  let
    vsnString =
      Vsn.vsnToString vsn

    url =
      "/packages/" ++ packageName ++ "/" ++ vsnString
  in
    a [ href url ] [ text vsnString ]

