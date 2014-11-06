module Markdown where
{-| A library for markdown parsing. This is just an Elm API built on top of the
[marked](https://github.com/chjj/marked) project which focuses on speed.
-}

import Native.Markdown
import Graphics.Element (Element)


toElement : String -> Element
toElement =
    Native.Markdown.toElement