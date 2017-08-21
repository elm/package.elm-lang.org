module Main exposing (..)


import Browser
import Browser.History as History
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Page.Docs as Docs
import Page.Problem as Problem
import Route
import Session
import Utils.App as App
import Version



-- MAIN


main =
  Browser.fullscreen
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onNavigation = Just onNavigation
    }



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


init : Browser.Env () -> ( Model, Cmd Msg )
init env =
  let
    targetRoute =
      Route.fromUrl env.url
  in
  goto targetRoute (Model Session.empty Blank (Just targetRoute))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- NAVIGATION


onNavigation : Browser.Url -> Msg
onNavigation url =
  Goto (Route.fromUrl url)



-- UPDATE


type Msg
  = Push Route.Route
  | Goto Route.Route
  | SessionMsg Session.Msg
  | DocsMsg Docs.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
  case message of
    Push route ->
      ( model
      , History.push (Route.toUrl route)
      )

    Goto route ->
      goto route model

    SessionMsg msg ->
      checkNext { model | session = Session.update msg model.session }

    DocsMsg msg ->
      case model.page of
        Docs docsModel ->
          let
            (newDocsModel, cmds) =
               Docs.update msg docsModel
          in
          ( { model | page = Docs newDocsModel }, cmds )

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
      , Cmd.none
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

    Route.NotFound url ->
      ( { model
            | page = Problem Problem.NoIdea
            , next = Nothing
        }
      , Cmd.none
      )


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

    Session.Loading newSessionData maybeReadme maybeDocs cmds ->
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


view : Model -> Browser.View Msg
view model =
  { title =
      toTitle model.page
  , body =
      [ center "#eeeeee" [ lazy2 viewHeader model.session model.page ]
      , center "#60B5CC" [ lazy2 viewVersionWarning model.session model.page ]
      , div [ class "center" ] (viewPage model.page)
      , viewFooter
      ]
  }


center : String -> List (Html msg) -> Html msg
center color kids =
  div [ style "background-color" color ] [ div [class "center"] kids ]


toTitle : Page -> String
toTitle page =
  case page of
    Blank ->
      "..."

    Problem suggestion ->
      Problem.toTitle suggestion

    Docs docsModel ->
      Docs.toTitle docsModel



-- VIEW PAGE


viewPage : Page -> List (Html Msg)
viewPage page =
  case page of
    Blank ->
      []

    Problem suggestion ->
      [ Html.map Push <| Problem.view suggestion
      ]

    Docs docsModel ->
      Docs.view DocsMsg docsModel



-- VIEW ROUTE LINKS


viewHeader : Session.Data -> Page -> Html Msg
viewHeader sessionData page =
  h1 [ class "header" ] <| (::) (App.link Push Route.Home [] [ viewLogo ]) <|
    case page of
      Blank ->
        []

      Problem _ ->
        []

      Docs { user, project, version, info } ->
        let (vsn, vsnStr) = moreExactVersion sessionData user project version in
        [ toLink (Route.User user) user
        , slash
        , toLink (Route.Package user project) project
        , slash
        , toLink (Route.Version user project vsn Route.Readme) vsnStr
        ]
        ++
          case info of
            Route.Readme ->
              []

            Route.Module moduleName maybeTag ->
              [ slash
              , toLink (Route.Version user project version (Route.Module moduleName maybeTag)) moduleName
              ]


slash : Html msg
slash =
  span [ class "spacey-char" ] [ text "/" ]


toLink : Route.Route -> String -> Html Msg
toLink route words =
  App.link Push route [] [ text words ]


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


viewVersionWarning : Session.Data -> Page -> Html Msg
viewVersionWarning session page =
  div [ class "header-underbar" ] <|
    case getNewerRoute session page of
      Nothing ->
        []

      Just (latestVersion, newerRoute) ->
        [ p [ class "version-warning" ]
            [ text "Warning! The latest version of this package is "
            , App.link Push newerRoute [] [ text (Version.toString latestVersion) ]
            ]
        ]


getNewerRoute : Session.Data -> Page -> Maybe (Version.Version, Route.Route)
getNewerRoute sessionData page =
  case page of
    Blank ->
      Nothing

    Problem _ ->
      Nothing

    Docs { user, project, version, info } ->
      case version of
        Route.Latest ->
          Nothing

        Route.Exactly vsn ->
          case Session.getLatestVersion sessionData user project of
            Nothing ->
              Nothing

            Just latestVersion ->
              if vsn == latestVersion then
                Nothing
              else
                Just ( latestVersion, Route.Version user project Route.Latest info )



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
