{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module ServeFile where

import Control.Monad.Trans (liftIO)
import qualified Data.List as List
import qualified Data.Text.Lazy as Text
import Snap.Core (Snap, writeBuilder)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Name as N
import qualified Elm.Package.Version as V
import qualified PackageSummary as PkgSummary
import qualified Path


filler :: Module.Name -> Snap ()
filler name =
    writeBuilder $
    Blaze.renderHtmlBuilder $
    docTypeHtml $ do 
      H.head $ do
        meta ! charset "UTF-8"
        H.title (toHtml (Module.nameToString name))
        H.style $ preEscapedToMarkup standardStyle
        script ! src (toValue ("/" ++ Path.artifact name)) $ ""

      body $
        script $ preEscapedToMarkup $
          "Elm.fullscreen(Elm." ++ Module.nameToString name ++ ")"

      analytics


packageDocs :: N.Name -> V.Version -> Snap ()
packageDocs pkg@(N.Name user name) version =
  do  maybeVersions <- liftIO (PkgSummary.readVersionsOf pkg)
      let versionList =
            maybe [] (List.map V.toString) maybeVersions

      writeBuilder $
        Blaze.renderHtmlBuilder $
        docTypeHtml $ do 
          H.head $ do
            meta ! charset "UTF-8"
            H.title (toHtml ("Elm Package Documentation" :: Text.Text))
            H.style $ preEscapedToMarkup standardStyle
            script ! src (toValue ("/artifacts/Page-PackageDocs.js" :: Text.Text)) $ ""

          body $ script $ preEscapedToMarkup $
              context
                [ ("user", show user)
                , ("name", show name)
                , ("version", show (V.toString version))
                , ("versionList", show versionList)
                ]
              ++ "var page = Elm.fullscreen(Elm.Page.PackageDocs, { context: context });\n"

          analytics


moduleDocs :: N.Name -> V.Version -> Module.Name -> Snap ()
moduleDocs pkg@(N.Name user name) version moduleName =
  do  maybeVersions <- liftIO (PkgSummary.readVersionsOf pkg)
      let versionList =
            maybe [] (List.map V.toString) maybeVersions

      writeBuilder $
        Blaze.renderHtmlBuilder $
        docTypeHtml $ do 
          H.head $ do
            meta ! charset "UTF-8"
            H.title (toHtml ("Elm Package Documentation" :: Text.Text))
            H.style $ preEscapedToMarkup standardStyle
            script ! src (toValue ("/artifacts/Page-ModuleDocs.js" :: Text.Text)) $ ""

          body $ script $ preEscapedToMarkup $
              context
                [ ("user", show user)
                , ("name", show name)
                , ("version", show (V.toString version))
                , ("versionList", show versionList)
                , ("moduleName", show (Module.nameToString moduleName))
                ]
              ++ "var page = Elm.fullscreen(Elm.Page.ModuleDocs, { context: context });\n"

          analytics


context :: [(String, String)] -> String
context pairs =
  "\nvar context = { " ++ List.intercalate ", " (List.map (\(k,v) -> k ++ ": " ++ v) pairs) ++ " };\n"


standardStyle :: Text.Text
standardStyle = 
    "html,head,body { padding:0; margin:0; }\n\
    \body { font-family: 'Lucida Grande','Trebuchet MS','Bitstream Vera Sans',Verdana,Helvetica,sans-serif; }\n\
    \a:link {text-decoration: none; color: rgb(15,102,230);}\n\
    \a:visited {text-decoration: none; color: rgb(15,102,230);}\n\
    \a:active {text-decoration: none}\n\
    \a:hover { text-decoration: underline; color: rgb(234,21,122); }\n\
    \h1,h2,h3,h4 { font-weight:normal; font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; }\n\
    \p, li {\n\
    \  font-size: 14px !important;\n\
    \  line-height: 1.5em !important;\n\
    \}\n\
    \pre {\n\
    \  margin-left: 30px;\n\
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