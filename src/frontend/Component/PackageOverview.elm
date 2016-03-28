module Component.PackageOverview exposing (..)

import Dict
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


init : Ctx.OverviewContext -> (Model, Cmd Msg)
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
        , Cmd.none
        )



-- UPDATE


type Msg
    = ExpandVersions Bool


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ExpandVersions bool ->
        ( { model | versionsAreExpanded = bool }
        , Cmd.none
        )



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view {context, versionDict, versionsAreExpanded} =
  div [ class "pkg-overview" ]
    [ h1 [] [text "Published Versions"]
    , p [] <|
        viewVersions context.user context.project versionsAreExpanded versionDict
        ++ expando versionsAreExpanded
    ]


expando : Bool -> List (Html Msg)
expando isExpanded =
  let
    msg =
      if isExpanded then "show fewer" else "show all"
  in
    text " — "
    :: [ a [ class "grey-link", onClick (ExpandVersions (not isExpanded)) ] [ text msg ] ]


viewVersions : String -> String -> Bool -> Vsn.Dictionary -> List (Html msg)
viewVersions user project isExpanded dict =
  Dict.toList dict
    |> List.concatMap (toRange user project isExpanded)
    |> List.intersperse (text (if isExpanded then ", " else " "))


toRange : String -> String -> Bool -> (Int, Vsn.MinorPatch) -> List (Html msg)
toRange user project isExpanded (major, {latest, others}) =
  if isExpanded then
    List.map (minorPatchToLink user project False major) others
    ++ [ minorPatchToLink user project True major latest ]

  else if List.isEmpty others then
    [ minorPatchToLink user project False major latest ]

  else
    [ text "…", minorPatchToLink user project False major latest ]


minorPatchToLink : String -> String -> Bool -> Int -> (Int, Int) -> Html msg
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
