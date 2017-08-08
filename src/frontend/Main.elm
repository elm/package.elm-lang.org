module Main exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)

import Page.Docs as Docs
import Route
import Version



-- MAIN



-- MODEL


type alias Model =
  { session : Session.Data
  , route : Route.Route
  , page : Page
  }


type Page
  = Blank
  | NotFound



-- INIT



-- UPDATE


type Msg
  = GoTo Route.Route
  | SessionMsg Session.Msg
  | DocsMsg Docs.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
  case message of
    GoTo newRoute ->
      goto newRoute model

    SessionMsg msg ->
      ( { model | session = Session.update msg model.session }
      , Cmd.none
      )

    DocsMsg msg ->
      Debug.crash "TODO"


goto : Route.Route -> Model -> ( Model, Cmd Msg )
goto route model =
  ( model, Cmd.none )



-- VIEW


view : Model -> Html msg
view model =
  div []
    [ center "#eeeeee" [ lazy2 viewLinks model.session model.page ]
    , center "#60B5CC" [ lazy2 viewVersionWarning modul.session model.page ]
    , div [ class "center" ] (lazy2 viewPage model.session model.page)
    , viewFooter
    ]


center : String -> List (Html msg) -> Html msg
center color kids =
  div [ style "background-color" color ] [ div [class "center"] kids ]



-- VIEW PAGE


viewPage : Page -> List (Html Msg)
viewPage page =
  case page of
    Blank ->
      []

    NotFound ->
      [ div
          [ style "height" "100%"
          , style "text-align" "center"
          , style "color" "#9A9A9A"
          , style "padding" "6em 0"
          ]
          [ div [ style "font-size" "12em" ] [ text "404" ]
          , div [ style "font-size" "3em" ] [ text "Page not found" ]
          ]
      ]



-- VIEW ROUTE LINKS


viewLinks : Session.Data -> Route.Route -> List (Html msg)
viewLinks sessionData route =
    case route of
      Route.Home ->
        []

      Route.User user ->
        [ link (Route.User user) user
        ]

      Route.Package user project ->
        [ link (Route.User user) user
        , link (Route.Package user project) project
        ]

      Route.Version user project vsn ->
        let (version, vsnStr) = moreExactVersion sessionData user project vsn in
        [ link (Route.User user) user
        , link (Route.Package user project) project
        , link (Route.Version user project version) vsnStr
        ]

      Route.Module user project vsn moduleName _ ->
        let (version, vsnStr) = moreExactVersion sessionData user project vsn in
        [ link (Route.User user) user
        , link (Route.Package user project) project
        , link (Route.Version user project version) vsnStr
        , link (Route.Module user project version moduleName Nothing) moduleName
        ]

      Route.Guidelines ->
        [ text "help" ]

      Route.DocsHelp ->
        [ text "help" ]

      Route.DocsPreview ->
        [ text "help" ]


link : Route.Route -> String -> Html msg
link route words =
  Route.link GoTo route [ text words ]


moreExactVersion : Session.Data -> String -> String -> Route.Version -> (Route.Version, String)
moreExactVersion sessionData user project vsn =
  case vsn of
    Route.Exactly version ->
      ( vsn, Version.toString version )

    Route.Latest ->
      case Session.getLatestVersion sessionData user project of
        Nothing ->
          ( vsn, "latest" )

        Just version ->
          ( Route.Exactly version, Version.toString version )



-- VIEW VERSION WARNINGS


viewVersionWarning : Session.Data -> Route -> Html msg
viewVersionWarning sessionData route =
  div [ class "header-underbar" ] <|
    case getNewerRoute sessionData route of
      Nothing ->
        []

      Just (latestVersion, newerRoute) ->
        [ p [ class "version-warning" ]
            [ text "Warning! The latest version of this package is "
            , Route.link newerRoute [] [ text (Version.toString latestVersion) ]
            ]
        ]


getNewerRoute : Session.Data -> Route -> Maybe (Version.Version, Route)
getNewerRoute sessionData route =
  case route of
    Home ->
      Nothing

    User _ ->
      Nothing

    Package _ _ ->
      Nothing

    Version user project version ->
      toLatestVersion sessionData user project version (\vsn -> Version user project vsn)

    Module user project version moduleName maybeValue ->
      toLatestVersion sessionData user project version (\vsn -> Module user project vsn moduleName maybeValue)

    Guidelines ->
      Nothing

    DocsHelp ->
      Nothing

    DocsPreview ->
      Nothing


toLatestVersion : Session.Data -> String -> String -> Version -> (Version -> Route) -> Maybe (Version.Version, Route)
toLatestVersion sessionData user project vsn makeRoute =
  case vsn of
    Latest ->
      Nothing

    Exactly version ->
      case Session.getLatestVersion sessionData user project of
        Nothing ->
          Nothing

        Just latestVersion ->
          if version == latestVersion then
            Nothing
          else
            Just (latestVersion, makeRoute Latest)



-- VIEW FOOTER


viewFooter : Html msg
viewFooter =
  div [class "footer"]
    [ text "All code for this site is open source and written in Elm. "
    , a [ class "grey-link", href "https://github.com/elm-lang/package.elm-lang.org/" ] [ text "Check it out" ]
    , text "! — © 2012-present Evan Czaplicki"
    ]



-- VIEW LOGO


viewLogo : Html msg
viewLogo =
  div
    [ style
        [ "display" => "-webkit-display"
        , "display" => "-ms-flexbox"
        , "display" => "flex"
        ]
    ]
    [ img
        [ src "/assets/elm_logo.svg"
        , style
            [ "height" => "30px"
            , "vertical-align" => "bottom"
            , "padding-right" => "8px"
            ]
        ]
        []
    , div
        [ style
          [ "color" => "black"
          ]
        ]
        [ div
            [ style
              [ "line-height" => "20px"
              ]
            ]
            [ text "elm" ]
        , div
            [ style
              [ "line-height" => "10px"
              , "font-size" => "0.5em"
              ]
            ]
            [ text "packages" ]
        ]
    ]
