module Overview.Constants
  ( toX
  , toFraction
  )
  where



-- CONSTANTS


outerWidth : Int
outerWidth =
  920


padding : Int
padding =
  30



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


