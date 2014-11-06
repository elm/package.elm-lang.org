{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module ServeFile where

import qualified Data.Text.Lazy as Text
import Snap.Core (Snap, writeBuilder)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import qualified Elm.Compiler.Module as Module
import qualified Path


moduleDocs :: Module.Name -> Snap ()
moduleDocs name =
    writeBuilder (Blaze.renderHtmlBuilder (moduleDocsHtml name))


moduleDocsHtml :: Module.Name -> Html
moduleDocsHtml name =
    docTypeHtml $ do 
      H.head $ do
        meta ! charset "UTF-8"
        H.title (toHtml (Module.nameToString name))
        H.style $ preEscapedToMarkup standardStyle
        script ! src (toValue ("/" ++ Path.artifact name)) $ ""

      body $ do
        script $
            preEscapedToMarkup ("Elm.fullscreen(Elm." ++ Module.nameToString name ++ ")")

      analytics


standardStyle :: Text.Text
standardStyle = 
    "html,head,body { padding:0; margin:0; }\n\
    \body { font-family: 'Lucida Grande','Trebuchet MS','Bitstream Vera Sans',Verdana,Helvetica,sans-serif; }\n\
    \a:link {text-decoration: none; color: rgb(15,102,230);}\n\
    \a:visited {text-decoration: none; color: rgb(15,102,230);}\n\
    \a:active {text-decoration: none}\n\
    \a:hover { text-decoration: underline; color: rgb(234,21,122); }\n\
    \h1,h2,h3,h4 { font-weight:normal; font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; }\n\
    \p, li { font-size: 14px !important;\n\
    \        line-height: 1.5em !important; }\n\
    \pre { background-color: white;\n\
    \      padding: 10px;\n\
    \      border: 1px solid rgb(216, 221, 225);\n\
    \      border-radius: 4px;\n\
    \}\n\
    \code > span.kw { color: #268BD2; }\n\
    \code > span.dt { color: #268BD2; }\n\
    \code > span.dv, code > span.bn, code > span.fl { color: #D33682; }\n\
    \code > span.ch { color: #DC322F; }\n\
    \code > span.st { color: #2AA198; }\n\
    \code > span.co { color: #93A1A1; }\n\
    \code > span.ot { color: #A57800; }\n\
    \code > span.al { color: #CB4B16; font-weight: bold; }\n\
    \code > span.fu { color: #268BD2; }\n\
    \code > span.re { }\n\
    \code > span.er { color: #D30102; font-weight: bold; }"


-- | Add analytics to a page.
analytics :: Html
analytics =
    script ! type_ "text/javascript" $
         "var _gaq = _gaq || [];\n\
         \_gaq.push(['_setAccount', 'UA-25827182-1']);\n\
         \_gaq.push(['_setDomainName', 'package.elm-lang.org']);\n\
         \_gaq.push(['_trackPageview']);\n\
         \(function() {\n\
         \  var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;\n\
         \  ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';\n\
         \  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);\n\
         \})();"