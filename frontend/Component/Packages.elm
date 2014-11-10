module Component.Packages where

import Basics (..)
import Color
import ColorScheme as C
import Graphics.Element (..)
import List
import List ((::))
import String
import Text


type alias Package =
    { name : String
    , summary : String
    , versions : [String]
    }


view : Int -> [Package] -> Element
view innerWidth packages =
    let bigWords =
          Text.fromString "Packages"
            |> Text.height 30
            |> Text.leftAligned

        header =
          container innerWidth 100 midLeft bigWords
    in
    flow down (header :: List.map (viewPackage innerWidth) packages)


viewPackage : Int -> Package -> Element
viewPackage innerWidth package =
  let pkgLink =
        Text.fromString package.name
          |> Text.link ("/packages/" ++ package.name ++ "/" ++ List.head package.versions)
          |> Text.leftAligned

      summary =
        Text.fromString package.summary
          |> Text.leftAligned
  in
      flow down
      [ color C.lightGrey (spacer innerWidth 1)
      , flow right
        [ container 200 36 midLeft pkgLink
        , container (innerWidth - 200) 36 midLeft summary
        ]
      ]