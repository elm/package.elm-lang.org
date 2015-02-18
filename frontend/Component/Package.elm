module Component.Package where

import Color
import ColorScheme as C
import Graphics.Element (..)
import List
import LocalChannel as LC
import String
import Text
import Markdown

import Component.Header as Header
import Component.Package.ModuleList as ModuleList
import Component.Package.Search as Search



view : LC.LocalChannel String -> LC.LocalChannel String -> Int -> ModuleList.Model -> String -> Maybe String -> Element
view versionChan searchChan w pkg searchTerm maybeReadme =
    flow down
    [ Header.view versionChan w pkg.user pkg.name pkg.version pkg.versionList Nothing
    , color C.lightGrey (spacer w 1)
    , spacer w 12
    , flow right
      [ spacer 30 5
      , ModuleList.view (w - 30 - 200 - 30) searchTerm pkg
      , Search.view searchChan 200 (ModuleList.isCore pkg) searchTerm
      ]
    , spacer w 12
    , color C.lightGrey (spacer w 1)
    , case maybeReadme of
        Nothing -> empty
        Just readme ->
          width w (Markdown.toElementWith options readme)
    ]


options : Markdown.Options
options =
  let defaults = Markdown.defaultOptions
  in
      { defaults | sanitize <- True }
