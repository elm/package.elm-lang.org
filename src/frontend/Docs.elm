module Docs where

import Dict

import Docs.Entry


type alias Package =
    { readme : Maybe String
    , modules : Dict.Dict String Module
    }


type alias Module =
    { name : String
    , comment : String
    , entries : Dict.Dict String Entry.Model
    }

