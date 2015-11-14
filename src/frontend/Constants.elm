module Constants where

import String

asset : String -> String
asset path =
  String.append "/assets/" path
