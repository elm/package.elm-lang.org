module Overview.Component.Links
  ( Model
  , init
  , update
  , Action
  , view
  )
  where

import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Docs.Version as Vsn
import Page.Context as Ctx
import Utils.Path exposing ((</>))



-- MODEL


type alias Model =
  { context : Ctx.OverviewContext
  , versionDict : Vsn.Dictionary
  }



-- INIT


init : Ctx.OverviewContext -> (Model, Effects Action)
init context =
  case Vsn.fromStringList context.versions of
    Err msg ->
      Debug.crash <|
        "One of the versions in "
        ++ toString context.versions
        ++ " has a problem: "
        ++ msg

    Ok allVersions ->
      ( Model context (Vsn.toDict allVersions)
      , Fx.none
      )



-- UPDATE


type Action
    = NoOp


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      ( model
      , Fx.none
      )



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view addr {context, versionDict} =
  div [ class "pkg-overview" ]
    [ h1 [] [ text ("History of " ++ context.user ++ "/" ++ context.project) ]
    , p [] <|
        viewVersions context.user context.project versionDict
    ]



-- JUMP TO VERSIONS


viewVersions : String -> String -> Vsn.Dictionary -> List Html
viewVersions user project dict =
  Dict.toList dict
    |> List.concatMap (toRange user project)
    |> List.intersperse (text ", ")


toRange : String -> String -> (Int, Vsn.MinorPatch) -> List Html
toRange user project (major, {latest, others}) =
  List.map (minorPatchToLink user project False major) others
  ++ [ minorPatchToLink user project True major latest ]


minorPatchToLink : String -> String -> Bool -> Int -> (Int, Int) -> Html
minorPatchToLink user project isBold major (minor, patch) =
  let
    version =
      Vsn.vsnToString (major, minor, patch)

    versionText =
      if isBold then
        span [ style [ "font-weight" => "bold" ] ] [text version]
      else
        text version
  in
    a [href ("/packages" </> user </> project </> version)] [versionText]


