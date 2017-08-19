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
  | RemovedModule String String Route.Version String



-- TITLE


toTitle : Suggestion -> String
toTitle _ =
  "Page Not Found!"



-- VIEW


view : Suggestion -> Html Route.Route
view suggestion =
  let
    (message, details) =
      viewSuggestion suggestion
  in
  div styles <|
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


viewSuggestion : Suggestion -> ( List (Html msg), List (Html Route.Route) )
viewSuggestion suggestion =
  case suggestion of
    NoIdea ->
      ( [ text "I cannot find this page!" ]
      , []
      )

    BadResource resourceError ->
      Debug.crash "TODO"

    RemovedModule user project vsn name ->
      ( [ text "I cannot find the "
        , code [] [ text name ]
        , text " module!"
        ]
      , [ p []
            [ text "Maybe it existed in a "
            , App.link identity (Route.Package user project) [] [ text "previous release" ]
            , text "? Maybe the "
            , App.link identity (Route.Version user project vsn Route.Readme) [] [ text "README" ]
            , text " will help you figure out what changed?"
            ]
        ]
      )
