module Page.Home where

import Color (..)
import Graphics.Element (..)
import Markdown
import Signal
import Text
import Window

import ColorScheme as C
import Component.TopBar as TopBar


port title : String
port title = "Elm Package Catalog"


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
  , color white <| container windowWidth 260 middle navigationBox
  , color C.mediumGrey (spacer windowWidth 1)
  , spacer windowWidth 20
  , flow right
    [ spacer ((windowWidth - 980) // 2) (windowHeight - TopBar.topBarHeight - 260 - 20 - 1)
    , width 450 left1
    , spacer 80 10
    , width 450 right1
    ]
  , spacer windowWidth 100
  ]


navigationBox : Element
navigationBox =
  flow down
  [ container 400 120 middle (image 360 120 "/assets/website-name.png")
  , flow right
    [ button "/packages" "All Packages"
    , button "/packages/elm-lang/core/latest" "Core Libraries"
    ]
  ]


button : String -> String -> Element
button href txt =
  Text.leftAligned (Text.color white (Text.fromString txt))
    |> container 158 38 middle
    |> color C.blue
    |> container 160 40 middle
    |> color (rgb 5 80 129)
    |> link href
    |> container 200 80 middle


left1 : Element
left1 = Markdown.toElement """

# Install Packages

Use [`elm-package`][elm-package] to install community packages:

[elm-package]: https://github.com/elm-lang/elm-package

```
elm-package install user/project
```

[`elm-package`][elm-package] is sandboxed by default. All packages are
installed on a per-project basis, so all your projects dependencies are
independent.

<br>

# Reliable Versioning

Thanks to Elm&rsquo;s design, `elm-package` is able to automatically enforce
versioning rules based on API changes. You can always compare APIs by running
a command like:

```
elm-package diff evancz/elm-html 1.0.0 1.1.0
```

This will show you all the values that were added, removed, and changed between
version 1.0.0 and 1.1.0. Based on these API diffs, `elm-package` enforces [a
restricted form of semver 2.0.0][rules], so PATCH changes really are PATCH
changes in Elm.

[rules]: https://github.com/elm-lang/elm-package#version-rules

"""

right1 : Element
right1 = Markdown.toElement """

# Design Guidelines

Before publishing packages with [`elm-package`][elm-package], look through the
[API Design Guidelines][design]. Some key takeaways are:

 * Design for a concrete use case
 * Use human readable names
 * Avoid gratuitous abstraction
 * [Write nice documentation][docs]

After looking through [the guidelines][design] carefully and [writing helpful
documentation][docs], publish your library with:

```
elm-package publish
```

For more information on publishing with `elm-package` check out
[the comprehensive usage overview](https://github.com/elm-lang/elm-package/blob/master/README.md).

[elm-package]: https://github.com/elm-lang/elm-package
[design]: /help/design-guidelines
[docs]: /help/documentation-format

"""
