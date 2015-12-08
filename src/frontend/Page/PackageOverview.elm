module Page.PackageOverview where

import Dict
import Effects as Fx
import Html exposing (..)
import Html.Attributes exposing (class, key, src, style)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy3)
import Json.Decode as Decode
import StartApp
import Task

import Docs.Entry as Entry
import Docs.Package as Docs
import Docs.Type as Type
import Docs.Version as Vsn
import Overview.Diff as Diff
import Overview.History as History
import Overview.Slider as Slider
import Page.Context as Ctx
import Parse.Type as Type
import Utils.Path exposing ((</>))
import Utils.ProximityTree as Prox



-- WIRES


port context : Ctx.OverviewContext


port rawHistory : List History.RawRelease


app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = []
    }


main =
  app.html


port worker : Signal (Task.Task Fx.Never ())
port worker =
  app.tasks



-- MODEL


type alias Model =
    { history : History.History
    , versions : Prox.ProximityTree Vsn.Version
    , slider1 : Slider.Model
    , slider2 : Slider.Model
    , docs : Dict.Dict Vsn.Version Docs
    }


type Docs
    = Loading
    | Failed Int
    | Ready (Docs.Package Type.Type)


init : ( Model, Fx.Effects Action )
init =
  let
    history =
      List.map History.processRaw rawHistory

    proxTree =
      Prox.map .version (Prox.fromList (toFloat << .date) history)

    (penultimate, ultimate) =
      latestInterestingVersions history
  in
    ( Model
        history
        proxTree
        (Slider.init (Prox.lookup penultimate proxTree))
        (Slider.init (Prox.lookup ultimate proxTree))
        (Dict.fromList [ penultimate => Loading, ultimate => Loading ])
    , Fx.batch [ loadDocs penultimate, loadDocs ultimate ]
    )


latestInterestingVersions : History.History -> (Vsn.Version, Vsn.Version)
latestInterestingVersions history =
  let
    interestingVersions =
      Vsn.filterInteresting (List.map .version history)
  in
    case List.reverse interestingVersions of
      latest :: sndLatest :: _ ->
        ( sndLatest, latest )

      [latest] ->
        ( Vsn.one, latest )

      [] ->
        Debug.crash "How can there be a published package with no versions?"



-- UPDATE


type Action
    = UpdateSlider1 Slider.Action
    | UpdateSlider2 Slider.Action
    | DocsFailed Vsn.Version
    | DocsLoaded Vsn.Version (Docs.Package Type.Type)


update : Action -> Model -> ( Model, Fx.Effects Action )
update action model =
  case action of
    UpdateSlider1 act ->
      let
        (newSlider, fx, maybeTarget) =
          Slider.update model.versions act model.slider1

        (newDocs, maybeRequest) =
          maybeLoadDocs model.docs maybeTarget
      in
        ( { model | slider1 = newSlider, docs = newDocs }
        , Fx.batch [ Fx.map UpdateSlider1 fx, maybeRequest ]
        )

    UpdateSlider2 act ->
      let
        (newSlider, fx, maybeTarget) =
          Slider.update model.versions act model.slider2

        (newDocs, maybeRequest) =
          maybeLoadDocs model.docs maybeTarget
      in
        ( { model | slider2 = newSlider, docs = newDocs }
        , Fx.batch [ Fx.map UpdateSlider2 fx, maybeRequest ]
        )

    DocsFailed vsn ->
      ( { model | docs = Dict.insert vsn (Failed 1) model.docs }
      , Fx.none
      )

    DocsLoaded vsn vsnDocs ->
      ( { model | docs = Dict.insert vsn (Ready vsnDocs) model.docs }
      , Fx.none
      )



-- SLIDERS


sliderInfo : Prox.ProximityTree Vsn.Version -> Slider.Model -> (Float, Vsn.Version)
sliderInfo versions slider =
  let
    fraction =
      Slider.currentFraction slider

    (_, version) =
      Prox.nearest fraction versions
  in
    (fraction, version)



-- EFFECTS


loadDocs : Vsn.Version -> Fx.Effects Action
loadDocs version =
  Ctx.getDocs { user = context.user, project = context.project, version = Vsn.vsnToString version }
    |> Task.map (DocsLoaded version << Docs.packageMap Type.parseWithFallback)
    |> flip Task.onError (always (Task.succeed (DocsFailed version)))
    |> Fx.task


maybeLoadDocs
    : Dict.Dict Vsn.Version Docs
    -> Maybe Vsn.Version
    -> (Dict.Dict Vsn.Version Docs, Fx.Effects Action)
maybeLoadDocs docs maybeTarget =
  case maybeTarget of
    Nothing ->
      (docs, Fx.none)

    Just version ->
      case Dict.get version docs of
        Nothing ->
          (Dict.insert version Loading docs, loadDocs version)

        Just (Failed _) ->
          (Dict.insert version Loading docs, loadDocs version)

        Just _ ->
          (docs, Fx.none)



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view address {history, versions, slider1, slider2, docs} =
  let
    (fraction1, version1) =
      sliderInfo versions slider1

    (fraction2, version2) =
      sliderInfo versions slider2

    viewSlider tag frac vsn color =
      Slider.view
        (Signal.forwardTo address tag)
        frac
        (Vsn.vsnToString vsn)
        color
  in
    div
      [ class "center"
      , style [ "padding-top" => "150px" ]
      ]
      [ History.view versions
      , div [ class "slider-container" ]
          [ viewSlider UpdateSlider1 fraction1 version1 "#7FD13B"
          , viewSlider UpdateSlider2 fraction2 version2 "#60B5CC"
          ]
      , div [ class "diff" ]
          [ diffHeader fraction1 fraction2 version1 version2
          , lazy3 coarseDiff history version1 version2
          , lazy3 detailedDiff docs version1 version2
          ]
      ]


diffHeader : Float -> Float -> Vsn.Version -> Vsn.Version -> Html
diffHeader fraction1 fraction2 version1 version2 =
  let
    text1 =
      vsnText "#7FD13B" version1

    text2 =
      vsnText "#60B5CC" version2

    (leftVsn, rightVsn) =
      if fraction1 < fraction2 then
        (text1, text2)

      else
        (text2, text1)
  in
    h1 [] [ text "Changes between ", leftVsn, text " and ", rightVsn ]


vsnText : String -> Vsn.Version -> Html
vsnText color vsn =
  span
    [ style
        [ "border-bottom" => ("4px solid " ++ color)
        ]
    ]
    [ text (Vsn.vsnToString vsn)
    ]



-- VIEW DIFFS


coarseDiff history version1 version2 =
  text (toString (History.diff version1 version2 history))


detailedDiff docs version1 version2 =
  let
    lowDocs =
      Dict.get (min version1 version2) docs

    highDocs =
      Dict.get (max version1 version2) docs
  in
    case Maybe.map2 (,) lowDocs highDocs of
      Just (Ready pkg1, Ready pkg2) ->
        detailedDiffHelp docs pkg1 pkg2

      Just (Loading, _) ->
        text "Loading.."

      Just (_, Loading) ->
        text "Loading.."

      _ ->
        text ""


detailedDiffHelp docs pkg1 pkg2 =
  let
    { added, changed, removed } =
      Diff.diffPackage pkg1 pkg2
  in
    ul [class "diff-detailed"] <|
      List.map (listItem "diff-removed-module" "removed") removed
      ++ List.map (listItem "diff-added-module" "added") added
      ++ viewPackageChanges changed


listItem itemClass verbed name =
  li [key name]
    [ header
        [class itemClass]
        [ code [] [text name], text (" was " ++ verbed ++ ".") ]
    ]


viewPackageChanges changes =
  List.map viewModuleChanges (Dict.toList changes)


viewModuleChanges (name, changes) =
  let
    changePairs =
      Dict.values changes.changed
  in
    li [key name]
      [ header [class "diff-changed-module"] [ code [] [ text name ] ]
      , ul [class "diff-removed"] (List.map viewEntry (Dict.values changes.removed))
      , ul [class "diff-added"] (List.map viewEntry (Dict.values changes.added))
      , ul [class "diff-changed"] (List.map viewChangePair changePairs)
      ]


viewChangePair (old, new) =
  li []
    [ viewTypeAnnotation old
    , viewTypeAnnotation new
    ]


viewEntry entry =
  li [] [viewTypeAnnotation entry]


viewTypeAnnotation entry =
  Entry.viewTypeAnnotation Dict.empty entry