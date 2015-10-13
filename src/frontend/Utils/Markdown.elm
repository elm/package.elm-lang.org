module Utils.Markdown (block) where

import Html
import Markdown


block : String -> Html.Html
block raw =
  Markdown.toHtmlWith myOptions raw


myOptions =
  let
    options =
      Markdown.defaultOptions
  in
    { options | defaultHighlighting = Just "elm" }
