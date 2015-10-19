module Component.Header where

import Component.DropDown exposing (dropdown)
import Graphics.Element exposing (..)
import Html
import Text

import ColorScheme as C


view : Signal.Address String -> Int -> String -> String -> String -> List String -> Maybe String -> Element
view versionChan innerWidth user package version versions maybeModule =
  let
    userLink =
      Text.link
        ("https://github.com/" ++ user)
        (Text.fromString user)

    packageLink =
      case maybeModule of
        Nothing -> Text.fromString package
        Just _ ->
          Text.link
            ("/packages/" ++ user ++ "/" ++ package ++ "/" ++ version)
            (Text.fromString package)

    userPackageText =
      userLink ++ Text.fromString " / " ++ packageLink

    headerText =
      case maybeModule of
        Nothing -> userPackageText
        Just name -> userPackageText ++ (Text.fromString (" / " ++ name))

    bigWords =
      headerText
        |> Text.height 24
        |> leftAligned

    githubLink =
      "https://github.com/" ++ user ++ "/" ++ package ++ "/tree/" ++ version

    viewSource =
      Text.fromString "Browse Source"
        |> Text.link githubLink
        |> Text.height 12
        |> centered

    toWarning latestVersion =
      if version == latestVersion then
        []
      else
        [ color C.lightGrey (spacer innerWidth 1)
        , container innerWidth 40 middle <| centered <|
            Text.fromString "Warning! The latest version of this package is "
            ++ Text.link ("/packages/" ++ user ++ "/" ++ package ++ "/" ++ latestVersion) (Text.fromString latestVersion)
        ]
  in
    flow down <|
      [ flow right
        [ container (innerWidth - 100) 100 midLeft bigWords
        , container 100 100 middle <|
            flow down
              [ Html.toElement 100 30 (dropdown versionChan version versions)
              , width 100 viewSource
              ]
        ]
      ]
      ++ Maybe.withDefault [] (Maybe.map toWarning (List.head versions))

