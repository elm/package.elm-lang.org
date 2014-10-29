module Home where

import String
import Website.ColorScheme as C
import Website.Skeleton (home)
import Window

port title : String
port title = "Elm Public Library"

padCol w col =
    let col' = col (w-40) in
    container (w-40) (heightOf col') middle col'

scene w =
    flow down
    [ spacer w 20
    , flow right [ padCol (w // 2) leftCol
                 , spacer 80 10
                 , padCol (w // 2) rightCol ]
    ]

--The Public Library is the central catalog of libraries created by the Elm community.
--It makes it easy to [discover libraries, browse documentation, and share ideas](/catalog).

leftCol w = width w [markdown|
<style>
h1,h2,h3,h4 {font-weight:normal;}
pre { background-color: white;
      padding: 10px;
      border: 1px solid rgb(216, 221, 225);
      border-radius: 4px;
}
</style>

# Install Libraries

Use [`elm-get`](https://github.com/elm-lang/elm-get) to install libraries:

```
elm-get install user/project
```

[`elm-get`](https://github.com/elm-lang/elm-get) is sandboxed by default,
installing on a per-project basis. It is also backed by [GitHub](https://github.com/),
so you can use it to install any public repo.

|]

rightCol w = width w [markdown|
<style>
h1,h2,h3,h4 {font-weight:normal;}
pre { background-color: white;
      padding: 10px;
      border: 1px solid rgb(216, 221, 225);
      border-radius: 4px;
}
li { padding: 2px; }
</style>

# Design Guidelines

Before publishing libraries with [`elm-get`](https://github.com/elm-lang/elm-get),
look through the [Library Design Guidelines](/DesignGuidelines.html). Some
key takeaways are:

 * Design for a concrete use case
 * Use human readable names
 * Avoid gratuitous abstraction
 * Use [semantic versioning](http://semver.org)
 * Write [nice documentation](/Documentation.html)

After looking through [the guidelines](/DesignGuidelines.html) carefully
and [writing helpful documentation](/Documentation.html), publish your library with:

```
elm-get publish
```

|]

main = home scene
