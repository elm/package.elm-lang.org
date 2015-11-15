module Docs.Summary (Summary, decoder) where

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
  object3
    Summary
    ("name" := string)
    ("summary" := string)
    ("versions" := list Vsn.decoder)

