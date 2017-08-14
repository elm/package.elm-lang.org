module Page.Problem exposing
  ( Suggestion(..)
  , view
  )


import Http
import Route
import Session.Resource as Resource



-- SUGGESTION


type Suggestion
  = NoIdea
  | BadResource Resource.Error
  | RemovedModule String String Route.Version String



-- VIEW


view : Suggestion -> Html msg
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

    RemovedModule user project vsn name ->
      ( [ text "I cannot find the "
        , code [] [ text name ]
        , text " module!"
        ]
      , [ p []
            [ text "Maybe it existed in a "
            , App.link identity (Route.Package user project) [] [ text "previous release" ]
            , text "? Maybe the "
            , App.link identity (Route.Readme user project vsn) [] [ text "README" ]
            , text " will help you figure out what changed?"
            ]
        ]
      )
