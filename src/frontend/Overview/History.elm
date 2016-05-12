module Overview.History
    ( History
    , Release
    , Magnitude
    , RawRelease, processRaw
    , diff
    , view
    )
    where

import Dict
import Html exposing (..)
import Html.Attributes exposing (class, classList, style)
import Html.Lazy exposing (lazy)
import Json.Decode as Json exposing ((:=))

import Docs.Version as Vsn
import Overview.Constants as Constants
import Utils.Path exposing ((</>))
import Utils.ProximityTree as Prox exposing (ProximityTree)



-- HISTORY


type alias History =
    List Release


type alias Release =
    { version : Vsn.Version
    , magnitude : Magnitude
    , date : Int
    }


type alias Magnitude =
    { added : Int
    , changed : Int
    , removed : Int
    }



-- PORT CONVERSIONS


type alias RawRelease =
    { version : String
    , added : Int
    , changed : Int
    , removed : Int
    , date : Int
    }


processRaw : RawRelease -> Release
processRaw raw =
  let
    version =
      case Vsn.fromString raw.version of
        Err _ ->
          Debug.crash "invalid version listed in history"

        Ok vsn ->
          vsn
  in
    { version = version
    , magnitude =
        { added = raw.added
        , changed = raw.changed
        , removed = raw.removed
        }
    , date = raw.date
    }



-- COARSE DIFF


diff : Vsn.Version -> Vsn.Version -> History -> Magnitude
diff v1 v2 history =
  diffHelp (min v1 v2) (max v1 v2) history (Magnitude 0 0 0)


diffHelp : Vsn.Version -> Vsn.Version -> History -> Magnitude -> Magnitude
diffHelp low high history magnitude =
  case history of
    [] ->
      magnitude

    release :: rest ->
      if release.version > high then
        magnitude

      else if release.version > low then
        diffHelp low high rest (addMagnitude magnitude release.magnitude)

      else
        diffHelp low high rest magnitude


addMagnitude : Magnitude -> Magnitude -> Magnitude
addMagnitude m1 m2 =
  Magnitude
    (m1.added + m2.added)
    (m1.changed + m2.changed)
    (m1.removed + m2.removed)



-- VIEW


(=>) = (,)


view : ProximityTree Vsn.Version -> Html
view releases =
  lazy viewHelp releases


viewHelp : ProximityTree Vsn.Version -> Html
viewHelp proxTree =
  let
    versions =
      Prox.toList proxTree

    nakedVersions =
      List.map snd versions

    dots =
      List.map3
        makeDot
        (versionZero :: nakedVersions)
        versions
        (List.map Just (List.drop 1 nakedVersions) ++ [Nothing])
  in
    div [ class "timeline" ] (line :: dots)


line : Html
line =
  div [class "timeline-line"] []


versionZero : Vsn.Version
versionZero =
  ( 0, 0, 0 )


makeDot : Vsn.Version -> (Float, Vsn.Version) -> Maybe Vsn.Version -> Html
makeDot before (fraction, current) maybeAfter =
  let
    isImportant =
      case maybeAfter of
        Nothing ->
          True

        Just after ->
          Vsn.Major == Vsn.magnitude current after

    x =
      Constants.toX fraction
  in
    case Vsn.magnitude before current of
      Vsn.Major ->
        dot x isImportant major (Just (Vsn.getMajor current))

      Vsn.Minor ->
        dot x isImportant minor Nothing

      Vsn.Patch ->
        dot x isImportant patch Nothing


dot : Int -> Bool -> List (String, String) -> Maybe Int -> Html
dot x isImportant magnitudeStyles maybeMajor =
  let
    classes =
      [ "timeline-dot" => True
      , "timeline-dot-important" => isImportant
      ]

    props =
      ("left" => px x) :: magnitudeStyles

    children =
      case maybeMajor of
        Nothing ->
          []

        Just major ->
          [ text (toString major) ]
  in
    div [ classList classes, style props ] children


px : Int -> String
px n =
  toString n ++ "px"


major : List (String, String)
major =
  let
    size = Constants.dotSize Vsn.Major Constants.WithoutBorder
  in
    [ "background-color" => "#eeeeee"
    , "border" => ((px Constants.dotBorderThickness) ++ " solid #bbbbbb")
    , "width" => px size
    , "height" => px size
    , "line-height" => px size
    , "font-size" => "14px"
    ]


minor : List (String, String)
minor =
  let
    size = Constants.dotSize Vsn.Minor Constants.WithoutBorder
  in
    [ "background-color" => "#eeeeee"
    , "border" => ((px Constants.dotBorderThickness) ++ " solid #bbbbbb")
    , "width" => px size
    , "height" => px size
    ]


patch : List (String, String)
patch =
  let
    size = Constants.dotSize Vsn.Patch Constants.WithoutBorder
  in
    [ "background-color" => "#bbbbbb"
    , "width" => px size
    , "height" => px size
    ]

