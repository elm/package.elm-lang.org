module Overview.Constants
  ( toX
  , toFraction
  , activeWidth
  , dotBorderThickness
  , dotSize
  , DotMetrics(WithBorder, WithoutBorder)
  )
  where

import Docs.Version as Vsn


-- CONSTANTS


outerWidth : Int
outerWidth =
  920


padding : Int
padding =
  30


-- TIMELINE DOTS

dotBorderThickness : Int
dotBorderThickness =
  4


type DotMetrics
  = WithBorder
  | WithoutBorder


dotSize : Vsn.Magnitude -> DotMetrics -> Int
dotSize magnitude metrics =
  let
    nakedSize =
      case magnitude of
        Vsn.Major -> 24
        Vsn.Minor -> 12
        Vsn.Patch -> 10

    borderSize =
      case metrics of
        WithBorder -> dotBorderThickness
        WithoutBorder -> 0
  in
    nakedSize + borderSize


-- CONVERSIONS

activeWidth : Float
activeWidth =
  toFloat (outerWidth - 2 * padding)


toX : Float -> Int
toX fraction =
  padding + round (fraction * activeWidth)


toFraction : Int -> Float
toFraction offset =
  toFloat offset / activeWidth


