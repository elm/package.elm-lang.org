module Docs.Summary exposing (Summary, decoder)

import Docs.Version as Vsn
import Json.Decode exposing (..)



type alias Summary =
    { name : String
    , summary : String
    , versions : List Vsn.Version
    }


decoder : Decoder (List Summary)
decoder =
  list decodeSummary


decodeSummary : Decoder Summary
decodeSummary =
  map3
    Summary
    (field "name" string)
    (field "summary" string)
    (field "versions" (list Vsn.decoder))

