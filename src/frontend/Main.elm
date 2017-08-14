module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Page.Docs as Docs
import Page.Problem as Problem
import Route
import Session
import Version



-- MAIN


main =
  Navigation.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
  { session : Session.Data
  , page : Page
  , next : Maybe Route.Route
  }


type Page
  = Blank
  | Problem Problem.Suggestion
  | Docs Docs.Model



-- INIT


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
  case Route.fromLocation location of
    Nothing ->
      ( Model Session.empty (Problem Problem.NoIdea) Nothing )

    Just route ->
      goto route (Model Session.empty Blank Nothing)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- UPDATE


type Msg
  = GoTo Route.Route
  | SessionMsg Session.Msg
  | DocsMsg Docs.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
  case message of
    GoTo route ->
      goto route model

    SessionMsg msg ->
      checkNext { model | session = Session.update msg model.session }

    DocsMsg msg ->
      case model.page of
        Docs docsModel ->
          case Docs.update msg docsModel of
            App.GoTo route ->
              goto route model

            App.Update newDocsModel ->
              ( { model | page = Docs newDocsModel }, Cmd.none )

        _ ->
          ( model, Cmd.none )



-- ROUTING


checkNext : Model -> ( Model, Cmd Msg )
checkNext model =
  case model.next of
    Nothing ->
      ( model, Cmd.none )

    Just route ->
      goto route model


goto : Route.Route -> Model -> ( Model, Cmd Msg )
goto route model =
  let
    gotoBlank =
      ( { model | page = Blank, next = Nothing }
      , Navigation.newUrl (Route.toString route)
      )
  in
  case route of
    Route.Home ->
      gotoBlank

    Route.User name ->
      gotoBlank

    Route.Package user project ->
      gotoBlank

    Route.Version user project version info ->
      loadVersion user project version info model

    Route.Guidelines ->
      gotoBlank

    Route.DocsHelp ->
      gotoBlank


loadVersion : String -> String -> Route.Version -> Route.VersionInfo -> Model -> ( Model, Cmd Msg )
loadVersion user project version info model =
  let
    makeDocsPage maybeReadme maybeDocs =
      Docs (Docs.Model user project version info maybeReadme maybeDocs "")
  in
  case Session.load user project version info model.session of
    Session.Done readme docs ->
      ( { model
          | page = makeDocsPage (Just readme) (Just docs)
          , next = Nothing
        }
      , Cmd.none
      )

    Session.Problem suggestion ->
      ( { model
          | page = Problem suggestion
          , next = Nothing
        }
      , Cmd.none
      )

    Session.Progress newSessionData maybeReadme maybeDocs cmds ->
      ( { model
            | session = newSessionData
            , page =
                case (info, maybeReadme, maybeDocs) of
                  (Route.Readme, Just _, _) ->
                    makeDocsPage maybeReadme maybeDocs

                  (Route.Module _ _, _, Just _) ->
                    makeDocsPage maybeReadme maybeDocs

                  _ ->
                    model.page
        }
      , Cmd.map SessionMsg cmds
      )



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


viewPage : Session.Data -> Page -> List (Html Msg)
viewPage session page =
  case page of
    Blank ->
      []

    Problem suggestion ->
      [ Problem.view suggestion
      ]

    Docs docsModel ->
      Docs.view session docsModel



-- VIEW ROUTE LINKS


viewLinks : Session.Data -> Route.Route -> List (Html msg)
viewLinks sessionData route =
    case route of
      Route.Home ->
        []

      Route.User user ->
        [ toLink (Route.User user) user
        ]

      Route.Package user project ->
        [ toLink (Route.User user) user
        , toLink (Route.Package user project) project
        ]

      Route.Version user project vsn ->
        let (version, vsnStr) = moreExactVersion sessionData user project vsn in
        [ toLink (Route.User user) user
        , toLink (Route.Package user project) project
        , toLink (Route.Version user project version) vsnStr
        ]

      Route.Module user project vsn moduleName _ ->
        let (version, vsnStr) = moreExactVersion sessionData user project vsn in
        [ toLink (Route.User user) user
        , toLink (Route.Package user project) project
        , toLink (Route.Version user project version) vsnStr
        , toLink (Route.Module user project version moduleName Nothing) moduleName
        ]

      Route.Guidelines ->
        [ text "help" ]

      Route.DocsHelp ->
        [ text "help" ]


toLink : Route.Route -> String -> Html msg
toLink route words =
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

    Readme user project version ->
      toLatestVersion sessionData user project version (\vsn -> Readme user project vsn)

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
    [ style "display" "-webkit-display"
    , style "display" "-ms-flexbox"
    , style "display" "flex"
    ]
    [ img
        [ src "/assets/elm_logo.svg"
        , style "height" "30px"
        , style "vertical-align" "bottom"
        , style "padding-right" "8px"
        ]
        []
    , div
        [ style "color" "black"
        ]
        [ div [ style "line-height" "20px" ] [ text "elm" ]
        , div
            [ style "line-height" "10px"
            , style "font-size" "0.5em"
            ]
            [ text "packages" ]
        ]
    ]
