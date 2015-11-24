module Docs.Package
  ( Package
  , Module
  , decodePackage
  , decodeModule
  )
  where

import Dict
import Json.Decode as Json exposing ((:=))

import Docs.Name as Name
import Docs.Entry as Entry



-- TYPES


type alias Package tipe =
    Dict.Dict String (Module tipe)


type alias Module tipe =
    { name : String
    , comment : String
    , entries : Dict.Dict String (Entry.Model tipe)
    , hasNonCanonicalTypes : Bool
    }



-- DECODERS


decodePackage : Json.Decoder (Package String)
decodePackage =
  Json.map (dictBy .name) (Json.list decodeModule)


decodeModule : Json.Decoder (Module String)
decodeModule =
  let
    make name comment values unions aliases hasNonCanonicalTypes =
      Module name comment (dictBy .name (values ++ unions ++ aliases)) hasNonCanonicalTypes
  in
    Json.object6 make
      ("name" := Json.string)
      ("comment" := Json.string)
      ("aliases" := Json.list (entry alias))
      ("types" := Json.list (entry union))
      ("values" := Json.list (entry value))
      ("generated-with-elm-version" := Json.map ((==) "old") Json.string)


dictBy : (a -> comparable) -> List a -> Dict.Dict comparable a
dictBy f list =
  Dict.fromList (List.map (\x -> (f x, x)) list)



-- ENTRY


entry : Json.Decoder (Entry.Info String) -> Json.Decoder (Entry.Model String)
entry decodeInfo =
  Json.object3
    Entry.Model
    ("name" := Json.string)
    decodeInfo
    ("comment" := Json.string)



-- VALUE INFO


value : Json.Decoder (Entry.Info String)
value =
  Json.object2
    Entry.Value
    ("type" := tipe)
    (Json.maybe fixity)


fixity : Json.Decoder Entry.Fixity
fixity =
  Json.object2
    Entry.Fixity
    ("precedence" := Json.int)
    ("associativity" := Json.string)



-- UNION INFO


union : Json.Decoder (Entry.Info String)
union =
  Json.map Entry.Union <|
    Json.object2
      Entry.UnionInfo
      ("args" := Json.list Json.string)
      ("cases" := Json.list tag)


tag : Json.Decoder (Entry.Tag String)
tag =
  Json.tuple2 Entry.Tag Json.string (Json.list tipe)



-- ALIAS INFO


alias : Json.Decoder (Entry.Info String)
alias =
  Json.map Entry.Alias <|
    Json.object2
      Entry.AliasInfo
      ("args" := Json.list Json.string)
      ("type" := tipe)



-- TYPES


tipe : Json.Decoder String
tipe =
  Json.string


