module Component.PackageOverview where

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
  , versionsAreExpanded : Bool
  }



-- INIT


init : Ctx.OverviewContext -> (Model, Effects Action)
init context =
  case Vsn.fromStringList context.versions of
    Err msg ->
      Debug.crash <| "One of the versions in " ++ toString context.versions ++ " has a problem: " ++ msg

    Ok allVersions ->
      let
        vsnDict =
          Vsn.toDict allVersions

        total =
          List.sum (List.map (\{others} -> 1 + List.length others) (Dict.values vsnDict))
      in
        ( Model context vsnDict (total < 10)
        , Fx.none
        )



-- UPDATE


type Action
    = ExpandVersions Bool


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    ExpandVersions bool ->
        ( { model | versionsAreExpanded = bool }
        , Fx.none
        )



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view addr {context, versionDict, versionsAreExpanded} =
  div [ class "pkg-overview" ]
    [ h1 [] [text "Published Versions"]
    , p [] <|
        viewVersions context.user context.project versionsAreExpanded versionDict
        ++ expando addr versionsAreExpanded
    ]


expando : Signal.Address Action -> Bool -> List Html
expando addr isExpanded =
  let
    msg =
      if isExpanded then "show fewer" else "show all"
  in
    text " — "
    :: [ a [ class "grey-link", onClick addr (ExpandVersions (not isExpanded)) ] [ text msg ] ]


viewVersions : String -> String -> Bool -> Vsn.Dictionary -> List Html
viewVersions user project isExpanded dict =
  Dict.toList dict
    |> List.concatMap (toRange user project isExpanded)
    |> List.intersperse (text (if isExpanded then ", " else " "))


toRange : String -> String -> Bool -> (Int, Vsn.MinorPatch) -> List Html
toRange user project isExpanded (major, {latest, others}) =
  if isExpanded then
    List.map (minorPatchToLink user project False major) others
    ++ [ minorPatchToLink user project True major latest ]

  else if List.isEmpty others then
    [ minorPatchToLink user project False major latest ]

  else
    [ text "…", minorPatchToLink user project False major latest ]


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
