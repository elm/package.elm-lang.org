module Component.Packages where

import Color
import ColorScheme as C
import Graphics.Element (..)
import Json.Decode (..)
import List
import List ((::))
import String
import Text


type alias Package =
    { name : String
    , summary : String
    , versions : List String
    }

package : Decoder Package
package =
    object3 Package
      ("name" := string)
      ("summary" := string)
      ("versions" := list string)


view : Int -> List Package -> Element
view innerWidth packages =
    let bigWords =
          Text.fromString "Packages"
            |> Text.height 30
            |> Text.leftAligned
    in
      flow down
      [ container innerWidth 100 midLeft bigWords
      , flow down (List.map (viewPackage innerWidth) packages)
      , spacer innerWidth 100
      ]


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
        [ container 300 36 midLeft pkgLink
        , container (innerWidth - 300) 36 midLeft summary
        ]
      ]