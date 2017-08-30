module Page.Problem exposing
  ( Suggestion(..)
  , view
  , toTitle
  )


import Html exposing (..)
import Html.Attributes exposing (..)
import Route
import Session.Resource as Resource
import Utils.App as App



-- SUGGESTION


type Suggestion
  = NoIdea
  | BadResource Resource.Error



-- TITLE


toTitle : Suggestion -> String
toTitle _ =
  "Page Not Found!"



-- VIEW


view : Suggestion -> App.Body Route.Route
view suggestion =
  case suggestion of
    NoIdea ->
      toBody [ text "I cannot find this page!" ] []

    BadResource resourceError ->
      case resourceError of
        Resource.MissingModule user project vsn name ->
          missingModule user project vsn name

        Resource.BadDocs user project vsn error ->
          cannotFind "docs.json"

        Resource.BadReadme user project vsn error ->
          cannotFind "README.md"

        Resource.BadReleases user project error ->
          cannotFind "releases.json"



-- CREATE BODY


toBody : List (Html msg) -> List (Html msg) -> App.Body msg
toBody message details =
  App.body styles <|
    [ div [ style "font-size" "12em" ] [ text "404" ]
    , div [ style "font-size" "3em" ] message
    ]
    ++ details


styles : List (Attribute msg)
styles =
  [ style "height" "100%"
  , style "text-align" "center"
  , style "color" "#9A9A9A"
  , style "padding" "6em 0"
  ]



-- HELPERS


cannotFind : String -> App.Body msg
cannotFind file =
  toBody [ text "Cannot find ", code [] [ text file ] ] []


missingModule : String -> String -> Route.Version -> String -> App.Body Route.Route
missingModule user project vsn name =
  toBody
    [ text "I cannot find the "
    , code [] [ text name ]
    , text " module!"
    ]
    [ p []
        [ text "Maybe it existed in a "
        , App.link identity (Route.Package user project) [] [ text "previous release" ]
        , text "? Maybe the "
        , App.link identity (Route.Version user project vsn Route.Readme) [] [ text "README" ]
        , text " will help you figure out what changed?"
        ]
    ]
