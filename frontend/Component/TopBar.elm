module Component.TopBar where

import Color
import ColorScheme as C
import Graphics.Element (..)
import Signal


type SearchScope
    = Package String
    | Global


type Status
    = Normal
    | Hover
    | Focus


type alias Model =
    { searchScope : SearchScope
    , query : String
    , status : Status
    }


type Update = NoOp


topBarHeight = 50

innerWidth = 980

logoSize = 28

searchBarWidth = 400


view : Int -> Signal.Channel Update -> Model -> Element
view outerWidth channel model =
  let leftPadding =
        (outerWidth - innerWidth) // 2

      rightPadding =
        outerWidth - innerWidth - leftPadding
  in
  flow right
  [ bar leftPadding empty
  , bar topBarHeight
      (link "/" (image logoSize logoSize "/assets/elm_logo.svg"))
  , bar searchBarWidth (searchBox model)
  , bar (innerWidth - topBarHeight - searchBarWidth + rightPadding) empty
  ]


bar : Int -> Element -> Element
bar fillerWidth elem =
  flow down
  [ color C.lightGrey (container fillerWidth topBarHeight middle elem)
  , color C.mediumGrey (spacer fillerWidth 1)
  ]


searchBox : Model -> Element
searchBox model =
  let boxColor =
        case model.status of
          Normal -> C.background
          Hover -> Color.white
          Focus -> Color.white
  in
    color C.mediumGrey <|
    container searchBarWidth (logoSize + 4) middle <|
    color boxColor <|
    container (searchBarWidth - 2) (logoSize + 2) midLeft <|
      case model.searchScope of
        Package pkg -> spacer 30 30
        Global -> spacer 30 30

