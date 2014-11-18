module Component.PackageDocs where

import Color
import ColorScheme as C
import Graphics.Element (..)
import List
import List ((::))
import String
import Text
import Markdown

type alias PackageInfo =
    { user : String
    , name : String
    , version : String
    , modules : List String
    }


view : Int -> PackageInfo -> Maybe String -> Element
view innerWidth pkg maybeReadme =
    let title =
          Text.fromString (pkg.user ++ " / " ++ pkg.name)
            |> Text.height 24
            |> Text.leftAligned

        href =
          "/packages/" ++ pkg.user ++ "/" ++ pkg.name

        metadata =
          Text.fromString (pkg.version ++ " - ") ++ Text.link href (Text.fromString "see other versions")
            |> Text.leftAligned

        header =
          flow right
          [ container (innerWidth // 2) 100 midLeft title
          , container (innerWidth - innerWidth // 2) 100 midRight metadata
          ]
    in
    flow down
    [ header
    , color C.lightGrey (spacer innerWidth 1)
    , spacer innerWidth 12
    , flow right
      [ spacer 30 5
      , flow down (List.map (viewModule (innerWidth - 60) pkg) pkg.modules)
      ]
    , spacer innerWidth 12
    , color C.lightGrey (spacer innerWidth 1)
    , case maybeReadme of
        Nothing -> empty
        Just readme ->
          width innerWidth (Markdown.toElement readme)
    ]


viewModule : Int -> PackageInfo -> String -> Element
viewModule innerWidth model name =
  let url =
        "/packages/" ++ model.user ++ "/" ++ model.name ++ "/" ++ model.version
        ++ "/" ++ String.map (\c -> if c == '.' then '-' else c) name
  in
      Text.fromString name
        |> Text.link url
        |> Text.leftAligned
        |> container innerWidth 24 midLeft
