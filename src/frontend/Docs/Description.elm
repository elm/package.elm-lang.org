module Docs.Description exposing (Description, Dependency, decoder)

import Json.Decode exposing (..)


type alias Dependency = (String, String)


type alias Description =
    { version : String
    , summary : String
    , repository : String
    , license : String
    , soruceDirectories : List String
    , exposedModules : List String
    , dependencies : List Dependency
    , elmVersion : String
    }


decoder : Decoder Description
decoder =
  object8
    Description
    ("version" := string)
    ("summary" := string)
    ("repository" := string)
    ("license" := string)
    ("source-directories" := list string)
    ("exposed-modules" := list string)
    ("dependencies" := keyValuePairs string)
    ("elm-version" := string)
