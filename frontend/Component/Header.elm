module Component.Header where

import Component.DropDown (dropdown)
import Graphics.Element (..)
import Html
import List
import List ((::))
import LocalChannel as LC
import Signal
import Text


view : LC.LocalChannel String -> Int -> String -> String -> String -> List String -> Maybe String -> Element
view versionChan innerWidth user package version versions maybeModule =
  let
    userLink =
      Text.link ("/packages/" ++ user) (Text.fromString user)

    packageLink =
      Text.link ("/packages/" ++ user ++ "/" ++ package ++ "/" ++ version) (Text.fromString package)

    userPackageText =
      userLink ++ Text.fromString " / " ++ packageLink

    headerText =
      case maybeModule of
        Nothing -> userPackageText
        Just name -> userPackageText ++ (Text.fromString (" / " ++ name))

    bigWords =
       headerText
        |> Text.height 24
        |> Text.leftAligned
  in
    flow right
    [ container (innerWidth - 100) 100 midLeft bigWords
    , container 100 100 middle <|
        Html.toElement 100 30 (dropdown versionChan version versions)
    ]
