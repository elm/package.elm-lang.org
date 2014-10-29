import Website.Skeleton (home)

port title : String
port title = "Documentation Format"

main = home scene

scene w =
    width (min w 800) words

words = [markdown|
<style>
pre {
  background-color: white;
  padding: 10px;
  border: 1px solid rgb(216, 221, 225);
  border-radius: 4px;
}
code > span.kw { color: #268BD2; }
code > span.dt { color: #268BD2; }
code > span.dv, code > span.bn, code > span.fl { color: #D33682; }
code > span.ch { color: #DC322F; }
code > span.st { color: #2AA198; }
code > span.co { color: #93A1A1; }
code > span.ot { color: #A57800; }
code > span.al { color: #CB4B16; font-weight: bold; }
code > span.fu { color: #268BD2; }
code > span.re { }
code > span.er { color: #D30102; font-weight: bold; }
</style>

# Documentation Format

This documentation format strives for simplicity and regularity. It should
be easy for readers to glance through a file and find the information they
need. Modules that are missing documentation cannot be uploaded to the catalog.

All documentation can use the same markdown as in Elm. You can check out
the [Maybe](https://github.com/elm-lang/Elm/blob/master/libraries/Maybe.elm)
and [Either](https://github.com/elm-lang/Elm/blob/master/libraries/Either.elm)
documentation for complete examples.

## Documenting a value

Here is an example from [the `String` library](/catalog/elm-lang-Elm/latest/String):

```haskell
{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['a','b','c'] == "abc"
-}
fromList : [Char] -> String
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

Here is the module documentation for [the `Maybe` library](/catalog/elm-lang-Elm/latest/Maybe):

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

|]
