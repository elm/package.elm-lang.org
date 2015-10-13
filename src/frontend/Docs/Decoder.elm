module Docs.Decoder (docs) where

import Json.Decode as Json exposing ((:=))

import Docs
import Docs.Name as Name
import Docs.Type as Type
import Docs.Entry as Entry


docs : Json.Decoder Docs.Model
docs =
  object5 (\name comment values unions aliases -> Docs.Model name comment (values ++ unions ++ aliases))
    ("name" := Json.string)
    ("comment" := Json.string)
    ("aliases" := Json.list (entry alias))
    ("types" := Json.list (entry union))
    ("values" := Json.list (entry value))



-- ENTRY

entry : Json.Decoder a -> Json.Decoder Entry.Model
entry decodeInfo =
  Json.object3 Entry.Model
    ("name" := Json.string)
    decodeInfo
    ("comment" := string)



-- VALUE


value : Json.Decoder Entry.Info
value =
  Json.object2 Entry.Value
    ("type" := tipe)
    (maybe fixity)


fixity : Json.Decoder Entry.Fixity
fixity =
  Json.object2 Entry.Fixity
    ("associativity" := Json.string)
    ("precedence" := Json.int)



-- UNION


union : Json.Decoder Entry.Info
union =
  Json.object2 (\vars tags -> Entry.Union { vars = vars, tags = tags })
    ("args" := Json.list Json.string)
    ("cases" := Json.list tag)


tag : Json.Decoder Entry.Tag
tag =
  Json.tuple2 Entry.Tag Json.string (Json.list tipe)



-- ALIAS


alias : Json.Decoder Entry.Info
alias =
  Json.object2 (\vars tipe -> Entry.Union { vars = vars, tipe = tipe })
    ("args" := Json.list Json.string)
    ("type" := tipe)


