module Component.Header where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)

import Docs.Version as Version
import Route exposing (..)



-- MODEL


type alias Model =
    { route : Route
    }


init : Route -> (Model, Effects a)
init route =
  ( Model route
  , Fx.none
  )



-- UPDATE


update : a -> Model -> (Model, Effects a)
update action model =
  (model, Fx.none)



-- VIEW


(=>) = (,)


view : Signal.Address a -> Model -> Html
view _ model =
  div
    []
    [ center "#eeeeee" [ headerLinks model ]
    , center "#60B5CC" (versionWarning model)
    ]


center color kids =
  div [ style [ "background-color" => color ] ]
    [ div [ class "center" ] kids
    ]



-- VIEW ROUTE LINKS


headerLinks model =
  h1 [ class "header" ] <|
    logo :: unrollRoute model.route


-- helpers

(</>) a b =
    a ++ "/" ++ b


spacey token =
  span [ class "spacey-char" ] [ text token ]


logo =
  a [ href "/", class "logo" ]
    [ img [ src "/assets/elm_logo.svg" ] []
    , text "elm"
    ]


headerLink url words =
  a [ href url, style [ "color" => "#333333" ] ]
    [ text words ]


-- route unrolling

unrollRoute : Route -> List Html
unrollRoute route =
  case route of
    Home ->
        []

    Help ->
        []

    Packages userRoute ->
        headerLink "/packages" "packages"
        :: maybe unrollUserRoute userRoute


maybe : (a -> List Html) -> Maybe a -> List Html
maybe unroll maybeRoute =
  case maybeRoute of
    Nothing ->
        []

    Just route ->
        unroll route


unrollUserRoute : UserRoute -> List Html
unrollUserRoute (User user packageRoute) =
    spacey "/"
    :: headerLink ("https://github.com" </> user) user
    :: maybe (unrollPackageRoute user) packageRoute


unrollPackageRoute : String -> PackageRoute -> List Html
unrollPackageRoute user (Package pkg versionRoute) =
    spacey "/"
    :: headerLink ("/packages" </> user </> pkg) pkg
    :: maybe (unrollVersionRoute user pkg) versionRoute


unrollVersionRoute : String -> String -> VersionRoute -> List Html
unrollVersionRoute user pkg (Version vsn allVersions) =
  [ spacey "/"
  , headerLink ("/packages" </> user </> pkg </> vsn) vsn
  ]



-- version warnings


versionWarning : Model -> List Html
versionWarning model =
  let
    warning =
      case model.route of
        Packages (Just (User user (Just (Package project (Just (Version vsn allVersions)))))) ->
            case Version.realMax vsn allVersions of
              Nothing ->
                []

              Just maxVersion ->
                [ p [ class "version-warning" ]
                    [ text "Warning! The latest version of this package is "
                    , a [ href ("/packages/" ++ user ++ "/" ++ project ++ "/" ++ maxVersion) ]
                        [ text maxVersion ]
                    ]
                ]

        _ ->
          []
  in
    [ div [ class "header-underbar" ] warning ]
