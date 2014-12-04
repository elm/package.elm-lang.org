module Page.DocumentationFormat where

import Graphics.Element (..)
import Markdown
import Signal
import Window

import ColorScheme as C
import Component.TopBar as TopBar


port title : String
port title = "Documentation Format"


main : Signal Element
main =
    Signal.map view Window.dimensions


search : Signal.Channel TopBar.Update
search =
    Signal.channel TopBar.NoOp


view : (Int,Int) -> Element
view (windowWidth, windowHeight) =
  color C.background <|
  flow down
  [ TopBar.view windowWidth search (TopBar.Model TopBar.Global "map" TopBar.Normal)
  , flow right
    [ spacer ((windowWidth - 980) // 2) (windowHeight - TopBar.topBarHeight)
    , width 600 content
    ]
  ]


content : Element
content = Markdown.toElement """

# Documentation Format

This documentation format strives for simplicity and regularity. It should
be easy for readers to glance through a file and find the information they
need. Modules that are missing documentation cannot be uploaded to the catalog.

All documentation can use the same markdown as in Elm. You can check out
the [Maybe](https://github.com/elm-lang/core/blob/master/src/Maybe.elm)
and [Result](https://github.com/elm-lang/core/blob/master/src/Result.elm)
documentation for complete examples.

## Documenting a value

Here is an example from [the `String` library](/catalog/elm-lang-Elm/latest/String):

```haskell
{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['a','b','c'] == "abc"
-}
fromList : List Char -> String
fromList = ...
```

Notice that:

  * A documentation comment starts `{-|` and ends with `-}`. The vertical
    bar indicates that it is a special comment.
  * The text begins after a single space, and all subsequent lines are
    aligned with the zeroth column.
  * Code is indented four spaces.
  * There is an example that shows a typical use of the function.
  * There is an explicit type annotation.

All of these things are necessary. Use this style when documenting your
publicly exposed functions. The goal is to have consistency across all
codebases, so readers can glance through easily and writers do not need
to argue about style.

## Documenting a module

Here is the module documentation for [the `Maybe` library](/packages/elm-lang/core/latest/Maybe):

```haskell
module Maybe (Maybe(..), maybe, isJust, isNothing, map) where

{-| Represents an optional value. Maybe it is there, maybe it is not.

# Type and Constructors
@docs Maybe

# Taking Maybes apart
@docs maybe, isJust, isNothing

# Map
@docs map
-}
```

This represents the text that actually gets displayed as [the
documentation](/catalog/elm-lang-Elm/latest/Maybe) for a module. Notice that:

  * The module documentation comes after the module declaration, but
    before the imports. This is so the first thing in the file is the
    module name and the second is how to use it.
  * The first line starts after a single space, and all subsequent lines
    start in the zeroth column.
  * The `@docs` keyword starts a list of values that are inlined in [the
    resulting documentation](/catalog/elm-lang-Elm/latest/Maybe).
  * Functions are grouped into related units with titles
  * Although documentation for each function should be self-contained,
    things are ordered intelligently. Assume people will read through
    linearly and try to make the document structure ideal for learning
    the API. You need to understand the Maybe data type to understand
    anything else, so it appears first. `maybe` is an important function
    so it appears early on. Etc.

Again, the goal is to have consistency, so readers can glance through easily
and writers do not need to argue about style.

"""