module Docs where

import Dict

import Docs.Entry as Entry


type alias Package =
    Dict.Dict String Module


type alias Module =
    { name : String
    , comment : String
    , entries : Dict.Dict String Entry.Model
    }


