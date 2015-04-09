module Component.TopBar where

import Color
import ColorScheme as C
import Graphics.Element exposing (..)
import Text


topBarHeight = 50

targetWidth = 980

logoSize = 28

searchBarWidth = 100


view : Int -> Element
view outerWidth =
  let innerWidth = min outerWidth targetWidth

      leftPadding =
        (outerWidth - innerWidth) // 2

      rightPadding =
        outerWidth - innerWidth - leftPadding
  in
  flow right
  [ bar leftPadding empty
  , bar logoSize
      (link "/" (image logoSize logoSize "/assets/elm_logo.svg"))
  , link "/packages" (bar searchBarWidth (leftAligned (Text.color (Color.rgb 5 80 129) (Text.fromString "Packages"))))
  , bar (innerWidth - logoSize - searchBarWidth + rightPadding) empty
  ]


bar : Int -> Element -> Element
bar fillerWidth elem =
  color C.blue (container fillerWidth topBarHeight middle elem)

