module Component.Header where

import Graphics.Element (..)
import Text


view : Int -> String -> String -> String -> Maybe String -> Element
view innerWidth user package version maybeModule =
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

    versionsLink =
      Text.link ("/packages/" ++ user ++ "/" ++ package) (Text.fromString "see other versions")

    versionsWords =
      Text.fromString (version ++ " - ") ++ versionsLink
        |> Text.leftAligned

    versionsWidth =
      widthOf versionsWords
  in
    flow right
    [ container (innerWidth - versionsWidth) 100 midLeft bigWords
    , container versionsWidth 100 middle versionsWords
    ]
