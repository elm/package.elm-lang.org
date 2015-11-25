module Docs.History where

import Html exposing (Html)
import Json.Decode as Json exposing ((:=))
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Docs.Version as Vsn



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


history : Json.Decoder History
history =
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


view : Signal.Address a -> History -> Html
view addr history =
  let
    (minDate, maxDate) =
      getRange .date .date history

    (minMag, maxMag) =
      getRange
        (\{magnitude} -> -magnitude.added)
        (\{magnitude} -> magnitude.changed + magnitude.removed)
        history

    timeline =
      makeTimeline (toY minMag maxMag 0)
  in
    svg
      [ width "920"
      , height "200"
      , viewBox "0 0 920 200"
      ]
      (timeline :: List.concatMap (viewRelease minDate maxDate minMag maxMag) history)


getRange : (a -> Int) -> (a -> Int) -> List a -> (Int, Int)
getRange toMin toMax history =
  Maybe.withDefault (0,0) <|
    Maybe.map2 (,)
      (List.minimum (List.map toMin history))
      (List.maximum (List.map toMax history))


makeTimeline : Int -> Svg
makeTimeline y =
  line
    [ x1 "0"
    , y1 (toString y)
    , x2 "920"
    , y2 (toString y)
    , stroke "grey"
    , strokeWidth "2"
    ]
    []



viewRelease : Int -> Int -> Int -> Int -> Release -> List Svg
viewRelease minDate maxDate minMag maxMag {version, date, magnitude} =
  let
    releaseX =
      toString (toX minDate maxDate date)

    zero =
      toString (toY minMag maxMag 0)

    bottom =
      toString (toY minMag maxMag -magnitude.added)

    middle =
      toString (toY minMag maxMag magnitude.changed)

    top =
      toY minMag maxMag (magnitude.changed + magnitude.removed)
  in
    [ circle [ cx releaseX, cy zero, r "4", fill "grey" ] []
    , makeLine releaseX bottom zero "#8ae234"
    , makeLine releaseX zero middle "#fcaf3e"
    , makeLine releaseX middle (toString top) "#cc0000"
    , text'
        [ x releaseX
        , y (toString (top - 10))
        , textAnchor "middle"
        ]
        [ text (Vsn.vsnToString version) ]
    ]


toX : Int -> Int -> Int -> Int
toX minDate maxDate releaseDate =
  20 + 880 * toFloat (releaseDate - minDate) / toFloat (maxDate - minDate)
    |> round


toY : Int -> Int -> Int -> Int
toY minMag maxMag mag =
  30 + 170 * toFloat (maxMag - mag) / toFloat (maxMag - minMag)
    |> round


makeLine : String -> String -> String -> String -> Svg
makeLine lineX bottomY topY color =
  line
    [ x1 lineX
    , y1 bottomY
    , x2 lineX
    , y2 topY
    , stroke color
    , strokeWidth "10"
    ]
    []