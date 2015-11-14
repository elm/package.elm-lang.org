module Docs.Summary (Package, decoder) where

import Docs.Version as Vsn
import Json.Decode exposing (..)



type alias Package =
    { name : String
    , summary : String
    , versions : List Vsn.Version
    }


decoder : Decoder (List Package)
decoder =
  list package


package : Decoder Package
package =
  object3
    Package
    ("name" := string)
    ("summary" := string)
    ("versions" := list Vsn.decoder)

