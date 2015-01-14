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
        googleAnalytics
        link ! rel "stylesheet" ! href "/assets/highlight/styles/default.css"
        script ! src "/assets/highlight/highlight.pack.js" $ ""
        script ! src (toValue ("/" ++ Path.artifact name)) $ ""

      body $
        script $ preEscapedToMarkup $
          "Elm.fullscreen(Elm." ++ Module.nameToString name ++ ")"



package :: N.Name -> V.Version -> Snap ()
package pkg@(N.Name user name) version =
  do  maybeVersions <- liftIO (PkgSummary.readVersionsOf pkg)
      let versionList =
            maybe [] (List.map V.toString) maybeVersions

      writeBuilder $
        Blaze.renderHtmlBuilder $
        docTypeHtml $ do 
          H.head $ do
            meta ! charset "UTF-8"
            H.title "Elm Package Documentation"
            H.style $ preEscapedToMarkup standardStyle
            googleAnalytics
            link ! rel "stylesheet" ! href "/assets/highlight/styles/default.css"
            script ! src "/assets/highlight/highlight.pack.js" $ ""
            script ! src "/artifacts/Page-Package.js" $ ""

          body $ script $ preEscapedToMarkup $
              context
                [ ("user", show user)
                , ("name", show name)
                , ("version", show (V.toString version))
                , ("versionList", show versionList)
                ]
              ++ "var page = Elm.fullscreen(Elm.Page.Package, { context: context });\n"


module' :: N.Name -> V.Version -> Module.Name -> Snap ()
module' pkg@(N.Name user name) version moduleName =
  do  maybeVersions <- liftIO (PkgSummary.readVersionsOf pkg)
      let versionList =
            maybe [] (List.map V.toString) maybeVersions

      writeBuilder $
        Blaze.renderHtmlBuilder $
        docTypeHtml $ do 
          H.head $ do
            meta ! charset "UTF-8"
            H.title "Elm Package Documentation"
            H.style $ preEscapedToMarkup standardStyle
            googleAnalytics
            link ! rel "stylesheet" ! href "/assets/highlight/styles/default.css"
            script ! src "/assets/highlight/highlight.pack.js" $ ""
            script ! src "/artifacts/Page-Module.js" $ ""

          body $ script $ preEscapedToMarkup $
              context
                [ ("user", show user)
                , ("name", show name)
                , ("version", show (V.toString version))
                , ("versionList", show versionList)
                , ("moduleName", show (Module.nameToString moduleName))
                ]
              ++
                "var page = Elm.fullscreen(Elm.Page.Module, { context: context });\n\
                \page.ports.docsLoaded.subscribe(function() { setTimeout(function() { window.location = window.location.hash; }, 0); });\n"


context :: [(String, String)] -> String
context pairs =
  "\nvar context = { " ++ List.intercalate ", " (List.map (\(k,v) -> k ++ ": " ++ v) pairs) ++ " };\n"


standardStyle :: Text.Text
standardStyle = 
    "html, head, body { padding:0; margin:0; }\n\
    \body { font-family: 'Lucida Grande','Trebuchet MS','Bitstream Vera Sans',Verdana,Helvetica,sans-serif; }\n\
    \a {\n\
    \  color: #1184CE;\n\
    \  text-decoration: none;\n\
    \}\n\
    \a:hover {\n\
    \  text-decoration: underline;\n\
    \  color: rgb(234,21,122);\n\
    \}\n\
    \h1,h2,h3,h4 { font-weight:normal; font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; }\n\
    \p, li {\n\
    \  font-size: 14px !important;\n\
    \  line-height: 1.5em !important;\n\
    \}\n\
    \pre {\n\
    \  margin: 0;\n\
    \  padding: 10px;\n\
    \  background-color: rgb(254,254,254);\n\
    \  border-style: solid;\n\
    \  border-width: 1px;\n\
    \  border-color: rgb(245,245,245);\n\
    \  border-radius: 6px;\n\
    \}\n"


-- | Add analytics to a page.
googleAnalytics :: Html
googleAnalytics =
    script ! type_ "text/javascript" $
        "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){\n\
        \(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),\n\
        \m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)\n\
        \})(window,document,'script','//www.google-analytics.com/analytics.js','ga');\n\
        \\n\
        \ga('create', 'UA-25827182-1', 'auto');\n\
        \ga('send', 'pageview');\n"
