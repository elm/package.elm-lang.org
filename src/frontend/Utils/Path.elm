module Utils.Path where

import String


hyphenate : String -> String
hyphenate string =
  String.map (\c -> if c == '.' then '-' else c) string


(</>) : String -> String -> String
(</>) a b =
    a ++ "/" ++ b




