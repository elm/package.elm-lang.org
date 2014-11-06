module Component.PackageDocs where

import Basics (..)
import Color
import ColorScheme as C
import Graphics.Element (..)
import List
import List ((::), (++))
import String
import Text
import Markdown

type alias Model =
    { user : String
    , package : String
    , version : String
    , modules : [String]
    , readme : String
    }


view : Int -> Model -> Element
view innerWidth model =
    let bigWords =
          Text.toText (model.user ++ " / " ++ model.package)
            |> Text.height 40
            |> Text.leftAligned

        header =
          container innerWidth 100 midLeft bigWords
    in
    flow down
    [ header
    , color C.lightGrey (spacer innerWidth 1)
    , spacer innerWidth 12
    , flow right
      [ spacer 30 5
      , flow down (List.map (viewModule (innerWidth - 60) model) model.modules)
      ]
    , spacer innerWidth 12
    , color C.lightGrey (spacer innerWidth 1)
    , width innerWidth (Markdown.toElement model.readme)
    ]


dummyModel =
    Model "elm-lang" "core" "1.0.0" ["List", "Signal", "Graphics.Element", "Graphics.Collage"] readme


viewModule : Int -> Model -> String -> Element
viewModule innerWidth model name =
  let url =
        "/packages/" ++ model.user ++ "/" ++ model.package ++ "/" ++ model.version
        ++ "/" ++ String.map (\c -> if c == '.' then '-' else c) name
  in
      Text.toText name
        |> Text.link url
        |> Text.leftAligned
        |> container innerWidth 30 midLeft


readme : String
readme = """

Learn about the Elm programming language at [elm-lang.org](http://elm-lang.org/).

[![Build Status](https://travis-ci.org/elm-lang/Elm.png)](https://travis-ci.org/elm-lang/Elm)

## Install

Follow [these instructions][installer] to use Elm on your machine. Be sure to use
the platform specific installers if you are on Mac or Windows. It's way easier!

 [installer]: https://github.com/elm-lang/elm-platform/blob/master/README.md#elm-platform 

## Build from source / Contribute

Use [this script][build] to build the entire Elm Platform from source: the compiler,
REPL, package manager, and reactor. Be sure to read all the instructions to learn
how the script works and what your workflow will be like.

[build]: https://github.com/elm-lang/elm-platform/blob/master/src/BuildFromSource.hs

## Help

If you are stuck, email
[the list](https://groups.google.com/forum/?fromgroups#!forum/elm-discuss)
or ask a question in the
[#Elm IRC channel](http://webchat.freenode.net/?channels=elm).

"""
