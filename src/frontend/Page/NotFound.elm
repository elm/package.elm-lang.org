module Page.NotFound exposing
  ( Suggestion(..)
  , view
  )



-- SUGGESTION


type Suggestion
  = NoIdea
  | RemovedModule String String Version.Version String



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

    RemovedModule user project version name ->
      let
        pkgRoute name =
          App.link identity (Route.Package user project) [] [ text name ]

        readmeRoute name =
          App.link identity (Route.Readme user project (Route.Exactly version)) [] [ text name ]
      in
      ( [ text "I cannot find the "
        , code [] [ text name ]
        , text " module!"
        ]
      , [ p []
            [ text "The "
            , pkgRoute (user ++ "/" ++ project)
            , text " package exists. Version "
            , readmeRoute (Version.toString version)
            , text " exists too."
        , p []
            [ text "Maybe it existed in a "
            , pkgRoute "previous release"
            , text "? Maybe the "
            , readmeRoute "README"
            , text " will help you figure out what changed?"
            ]
            -- TODO point people to the diffing tool to see when it was removed!
        ]
      ]
