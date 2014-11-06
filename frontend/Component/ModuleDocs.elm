module Component.ModuleDocs where

import Basics (..)
import ColorScheme as C
import Graphics.Element (..)
import Markdown
import Maybe
import List ((++))
import Text


view : Int -> String -> String -> Documentation -> Element
view innerWidth user package docs =
    let bigWords =
          Text.toText (user ++ " / " ++ package ++ " / " ++ docs.name)
            |> Text.height 40
            |> Text.leftAligned

        header =
          container innerWidth 100 midLeft bigWords
    in
    flow down
    [ header
    , color C.lightGrey (spacer innerWidth 1)
    , spacer innerWidth 12
    , width innerWidth (Markdown.toElement docs.comment)
    ]


dummyDocs =
    Documentation
        "Basics"
        comment
        []
        []
        [ Value "identity" identityComment (Lambda (Var "a") (Var "a")) Maybe.Nothing ]


comment = """
basic functions for stuff

@docs identity
"""

identityComment = """
Function that returns the argument unaltered

    identity 42 == 42
"""


-- MODEL OF DOCUMENTATION

type alias Documentation =
    { name : String
    , comment : String
    , aliases : [Alias]
    , unions : [Union]
    , values : [Value]
    }


type alias Alias =
    { name : String
    , comment : String
    , args : [String]
    , tipe : Type
    }


type alias Union =
    { name : String
    , comment : String
    , args : [String]
    , cases : [(String, [Type])]
    }


type alias Value =
    { name : String
    , comment : String
    , tipe : Type
    , assocPrec : Maybe.Maybe (String,Int)
    }


type Type
    = Lambda Type Type
    | Var String
    | Type String
    | App Type [Type]
    | Record [(String, Type)] (Maybe.Maybe Type)