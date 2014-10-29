module Website.Skeleton (skeleton,home) where

import Graphics.Input as Input
import Graphics.Input.Field as F
import Text
import Website.ColorScheme as C
import Window

headerHeight = 80
footerHeight = 60

search : Input.Input F.Content
search = Input.input F.noContent

searchWidth : Int
searchWidth = 140

searchStyle : F.Style
searchStyle = { padding = { top=0, bottom=0, left=6, right=6 }
              , outline = { color = C.mediumGrey
                          , width = F.uniformly 1
                          , radius = 4
                          }
              , highlight = F.noHighlight
              , style = Text.defaultStyle
              }

type BodyGen a = String -> a -> Int -> Element

skeleton : String -> [(String,Text)] -> BodyGen a -> Signal a -> Signal Element
skeleton ghostText links bodyFunc info =
    lift3 (internalSkeleton ghostText links bodyFunc) search.signal info Window.dimensions

internalSkeleton : String -> [(String,Text)] -> BodyGen a -> F.Content -> a -> (Int,Int) -> Element
internalSkeleton ghostText links bodyFunc term info (outer,h) =
    let margin = outer // 10
        inner = margin * 8
        leftGutter = max margin headerHeight
        content = bodyFunc term.string info (min inner (outer - leftGutter))
    in
    color C.lightGrey <|
    flow down
    [ topBar outer
    , flow right
      [ container leftGutter headerHeight middle <| link "http://elm-lang.org" <|
        container 50 50 middle <| image 50 50 "/resources/elm_logo_grey.svg"
      , container (inner - searchWidth - 20) headerHeight midLeft <|
        leftAligned <| Text.height 30 <| concat <| intersperse (toText " / ") <| (Text.link "/" <| toText "~") ::
        zipWith (<|) (repeat (length links) (uncurry Text.link) ++ [snd]) (("/catalog", toText "Catalog") :: links)
      , container (searchWidth + 20) headerHeight middle <| width searchWidth <|
        F.field searchStyle search.handle identity ghostText term
      ]
    , let contentHeight = max (heightOf content)
                              (h - topBarHeight - headerHeight - footerHeight)
      in  flow right [ spacer leftGutter contentHeight, content ]
    , footer outer
    ]

home : (Int -> Element) -> Signal Element
home bodyFunc = internalHome bodyFunc <~ Window.dimensions

internalHome : (Int -> Element) -> (Int,Int) -> Element
internalHome bodyFunc (outer,h) =
    let margin = outer // 10
        inner = outer - 2 * homeHeaderHeight
        content = bodyFunc inner
    in
    color C.lightGrey <|
    flow down
    [ topBar outer
    , homeHeader outer inner
    , let contentHeight = max (heightOf content)
                              (h - topBarHeight - homeHeaderHeight - footerHeight)
      in  container outer contentHeight (topLeftAt (absolute homeHeaderHeight) (absolute 0)) content
    , footer outer
    ]

clicks : Input.Input ()
clicks = Input.input ()

logoButton : Element
logoButton =
    let box c = color c <| container (tileSize) (tileSize) middle <| image 80 80 "/resources/elm_logo_grey.svg"
    in  Input.customButton clicks.handle () (box (rgb 57 59 58)) (box C.accent1) (box C.accent1)

browseButton : Element
browseButton =
    let box c = color c <| container 122 52 middle <|
                color C.accent1 <| container 120 50 middle <|
                leftAligned << Text.height 20 << Text.color C.lightGrey <| toText "Browse"
    in  Input.customButton clicks.handle () (box C.mediumGrey) (box C.lightGrey) (box white)

tileSize = 84
homeHeaderHeight = 3 * (tileSize // 2)
homeHeader outer inner =
    color (rgb 60 60 60) <| layers
    [ tiledImage outer homeHeaderHeight "/resources/tile.png"
    , flow right [ container homeHeaderHeight homeHeaderHeight middle <|
                   link "http://elm-lang.org" logoButton
                 , container (inner - 142) homeHeaderHeight midLeft title
                 , container 142 homeHeaderHeight middle <|
                   link "/catalog" browseButton
                 ]
    ]

bigWords = Text.height 40 <| Text.color C.mediumGrey <| toText "Elm Public Library "
alpha = Text.height 20 <| Text.color C.accent1 <| toText "ALPHA"
title =
    flow down
    [ link "/" <| leftAligned <| bigWords ++ alpha
    , spacer 10 4
    , leftAligned << Text.height 16 << Text.color C.mediumGrey <| toText "Discover libraries, browse documentation"
    ]

topBarHeight = 6
topBar outer =
    color C.accent1 <| spacer outer topBarHeight

footer outer = container outer footerHeight footerPosition <| Text.centered footerWords
footerPosition = midBottomAt (relative 0.5) (absolute 10)
footerWords =
  let wordLink words1 href words2 words3 =
          toText words1 ++ Text.link href (toText words2) ++ toText words3
  in
     Text.color (rgb 145 145 145) <|
       wordLink "written in Elm and " "https://github.com/elm-lang/elm-get/tree/master/website/content/src" "open source" "" ++
       wordLink " / " "https://github.com/evancz" "Evan Czaplicki" " &copy;2013-14"
