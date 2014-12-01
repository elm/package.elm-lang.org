
module Page.Error where

import ColorScheme as C
import Graphics.Element (..)
import Markdown
import Signal
import Text
import Window


main : Signal Element
main =
    Signal.map view Window.dimensions


view : (Int,Int) -> Element
view (w,h) =
  color C.mediumGrey <|
  container w h middle <|
    flow down
    [ color C.blue (spacer 360 5)
    , color C.lightGrey <| container 360 (heightOf error404) middle error404
    ]


error404 : Element
error404 = width 300 <| Markdown.toElement """

<h1><div>Poem 404
<div style="font-size:0.5em;font-weight:normal">Page Not Found</div></div>
</h1>

I shall be telling this with a sigh<br/>
Somewhere ages and ages hence:<br/>
Two roads diverged in a wood, and I&mdash;<br/>
I took the one less traveled by,<br/>
And that has made all the difference.

<p style="text-align:right;font-style:italic;">Robert Frost</p>

"""
