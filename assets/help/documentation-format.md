# Documentation Format

This documentation format strives for simplicity and regularity. It should be easy for readers to glance through a file and find the information they need. Modules that are missing documentation cannot be uploaded to the catalog.

All documentation can use the same markdown as in Elm. You can check out the [Maybe](https://github.com/elm/core/blob/master/src/Maybe.elm) and [Result](https://github.com/elm/core/blob/master/src/Result.elm) documentation for complete examples.

If you are preparing to publish a package, you should also read through the [API design guidelines](/help/design-guidelines)!


## Documenting a value

Here is an example from [the `String` library](/packages/elm/core/latest/String):

```elm
{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
fromList : List Char -> String
fromList = ...
```

Notice that:

  * A documentation comment starts `{-|` and ends with `-}`. The vertical bar indicates that it is a special comment.
  * The text begins after a single space, and all subsequent lines are aligned all the way to the left.
  * There is an example that shows a typical use of the function.
  * This example is indented four spaces.
  * The comment is closed on its own line.
  * There is an explicit type annotation.

For publicly exposed functions, type annotations and comments are required, and examples are best practice. The goal is to have consistency across all codebases, so readers can glance through easily and writers do not need to argue about style.

## Documenting a module

Here is the module documentation for [the `Maybe` library](/packages/elm/core/latest/Maybe):

```elm
module Maybe exposing (Maybe(Just,Nothing), andThen, map, withDefault, oneOf)

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Definition
@docs Maybe

# Common Helpers
@docs map, withDefault, oneOf

# Chaining Maybes
@docs andThen

-}
```

This represents the text that actually gets displayed as [the documentation](/packages/elm/core/latest/Maybe) for a module. Notice that:

  * The module lists what values are exported. Maybes are not an opaque type, as the tags are exported as well.
  * The module documentation comes after the module declaration, but before the imports. This is so the first thing in the file is the module name and the second is how to use it.
  * The first line starts after a single space, and all subsequent lines are aligned all the way to the left.
  * The `@docs` keyword precedes a comma-separated list of values that are inlined in [the resulting documentation](/packages/elm/core/latest/Maybe).
  * Functions are grouped into related units with titles, declared in Markdown. Sometimes it's appropriate to put a little text under the title associated with the group but not any one function.
  * Although documentation for each function should be self-contained, things are ordered intelligently. Assume people will read through linearly, so try to make the document structure ideal for learning the API. You need to understand the Maybe data type to understand anything else, so it appears first. `withDefault` is an important function so it appears early on. And so forth.

Again, the goal is to have consistency, so readers can glance through easily and writers do not need to argue about style.

Finally, modules need to be listed as exposed in `elm.json`. Some of the compiler's rules about the documentation format will only be enforced for these modules.
