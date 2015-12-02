module Overview.History
    ( History, Release, Magnitude
    , dummy
    , decoder
    , view, toX
    )
    where

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Json.Decode as Json exposing ((:=))

import Docs.Version as Vsn
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


dummy =
  [ Release (1,0,0) (Magnitude 581 0 0) 1418249334
  , Release (1,1,0) (Magnitude 4 0 0) 1420707342
  , Release (1,1,1) (Magnitude 0 0 0) 1424623511
  , Release (2,0,0) (Magnitude 27 6 82) 1429513536
  , Release (2,0,1) (Magnitude 0 0 0) 1430312693
  , Release (2,1,0) (Magnitude 3 0 0) 1433980479
  , Release (3,0,0) (Magnitude 37 4 12) 1448228740
  ]



-- DECODERS


decoder : Json.Decoder History
decoder =
  Json.list release


release : Json.Decoder Release
release =
  Json.object3
    Release
    ("version" := Vsn.decoder)
    magnitude
    ("date" := Json.int)


magnitude : Json.Decoder Magnitude
magnitude =
  Json.object3
    Magnitude
    ("added" := Json.int)
    ("changed" := Json.int)
    ("removed" := Json.int)



-- VIEW


(=>) = (,)


view : Int -> ProximityTree Release -> Html
view outerWidth releases =
  lazy2 viewHelp outerWidth releases


viewHelp : Int -> ProximityTree Release -> Html
viewHelp outerWidth proxTree =
  let
    versions =
      List.map (\(frac, release) -> (frac, release.version)) (Prox.toList proxTree)

    dots =
      List.map3
        (makeDot outerWidth)
        ((0, versionZero) :: versions)
        versions
        (List.map Just (List.drop 1 versions) ++ [Nothing])
  in
    div [ class "timeline" ] (line :: dots)


line : Html
line =
  div [class "timeline-line"] []


versionZero : Vsn.Version
versionZero =
  ( 0, 0, 0 )


padding : Int
padding =
  30


toX : Int -> Float -> Int
toX outerWidth fraction =
  padding + round (Debug.log "fraction" fraction * toFloat (outerWidth - 2 * padding))


makeDot : Int -> (Float, Vsn.Version) -> (Float, Vsn.Version) -> Maybe (Float, Vsn.Version) -> Html
makeDot outerWidth (_, before) (fraction, current) maybeAfter =
  let
    isImportant =
      case maybeAfter of
        Nothing ->
          True

        Just (_, after) ->
          Vsn.Major == Vsn.magnitude current after

    x =
      toX outerWidth fraction
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
  [ "background-color" => "#eeeeee"
  , "border" => "4px solid #bbbbbb"
  , "width" => "20px"
  , "height" => "20px"
  , "line-height" => "20px"
  , "font-size" => "14px"
  ]


minor : List (String, String)
minor =
  [ "background-color" => "#eeeeee"
  , "border" => "4px solid #bbbbbb"
  , "width" => "8px"
  , "height" => "8px"
  ]


patch : List (String, String)
patch =
  [ "background-color" => "#bbbbbb"
  , "width" => "10px"
  , "height" => "10px"
  ]

