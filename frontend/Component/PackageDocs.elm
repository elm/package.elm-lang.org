module Component.PackageDocs where

import Color
import ColorScheme as C
import Graphics.Element (..)
import List
import List ((::))
import LocalChannel as LC
import String
import Text
import Markdown

import Component.Header as Header


type alias PackageInfo =
    { user : String
    , name : String
    , version : String
    , versionList : List String
    , modules : List String
    }


view : LC.LocalChannel String -> Int -> PackageInfo -> Maybe String -> Element
view versionChan innerWidth pkg maybeReadme =
    flow down
    [ Header.view versionChan innerWidth pkg.user pkg.name pkg.version pkg.versionList Nothing
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
