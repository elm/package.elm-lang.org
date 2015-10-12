{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module ServeFile where

import Control.Monad.Trans (liftIO)
import qualified Data.List as List
import Snap.Core (Snap, writeBuilder)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import qualified PackageSummary as PkgSummary
import qualified Path


favicon :: H.Html
favicon =
  H.link
    ! A.rel "shortcut icon"
    ! A.size "16x16, 32x32, 48x48, 64x64, 128x128, 256x256"
    ! A.href "/assets/favicon.ico"


filler :: Module.Name -> Snap ()
filler name =
    writeBuilder $
    Blaze.renderHtmlBuilder $
    docTypeHtml $ do
      H.head $ do
        meta ! charset "UTF-8"
        favicon
        H.title (toHtml (Module.nameToString name))
        googleAnalytics
        link ! rel "stylesheet" ! href "/assets/highlight/styles/default.css"
        link ! rel "stylesheet" ! href "/assets/style.css"
        script ! src "/assets/highlight/highlight.pack.js" $ ""
        script ! src (toValue ("/" ++ Path.artifact name)) $ ""

      body $
        script $ preEscapedToMarkup $
          "Elm.fullscreen(Elm." ++ Module.nameToString name ++ ")"



package :: Pkg.Name -> Pkg.Version -> Snap ()
package pkg@(Pkg.Name user name) version =
  do  maybeVersions <- liftIO (PkgSummary.readVersionsOf pkg)
      let versionList =
            maybe [] (List.map Pkg.versionToString) maybeVersions

      writeBuilder $
        Blaze.renderHtmlBuilder $
        docTypeHtml $ do
          H.head $ do
            meta ! charset "UTF-8"
            favicon
            H.title "Elm Package Documentation"
            googleAnalytics
            link ! rel "stylesheet" ! href "/assets/highlight/styles/default.css"
            link ! rel "stylesheet" ! href "/assets/style.css"
            script ! src "/assets/highlight/highlight.pack.js" $ ""
            script ! src "/artifacts/Page-Package.js" $ ""

          body $ script $ preEscapedToMarkup $
              context
                [ ("user", show user)
                , ("name", show name)
                , ("version", show (Pkg.versionToString version))
                , ("versionList", show versionList)
                ]
              ++ "var page = Elm.fullscreen(Elm.Page.Package, { context: context });\n"


module' :: Pkg.Name -> Pkg.Version -> Module.Name -> Snap ()
module' pkg@(Pkg.Name user name) version moduleName =
  do  maybeVersions <- liftIO (PkgSummary.readVersionsOf pkg)
      let versionList =
            maybe [] (List.map Pkg.versionToString) maybeVersions

      writeBuilder $
        Blaze.renderHtmlBuilder $
        docTypeHtml $ do
          H.head $ do
            meta ! charset "UTF-8"
            favicon
            H.title "Elm Package Documentation"
            googleAnalytics
            link ! rel "stylesheet" ! href "/assets/highlight/styles/default.css"
            link ! rel "stylesheet" ! href "/assets/style.css"
            script ! src "/assets/highlight/highlight.pack.js" $ ""
            script ! src "/artifacts/Page-Module.js" $ ""

          body $ script $ preEscapedToMarkup $
              context
                [ ("user", show user)
                , ("name", show name)
                , ("version", show (Pkg.versionToString version))
                , ("versionList", show versionList)
                , ("moduleName", show (Module.nameToString moduleName))
                ]
              ++
                "var page = Elm.fullscreen(Elm.Page.Module, { context: context });\n\
                \page.ports.docsLoaded.subscribe(function() {\n\
                \    if (window.location.hash) {\n\
                \        setTimeout(function() { window.location = window.location.hash; }, 0);\n\
                \    }\n\
                \});\n"


context :: [(String, String)] -> String
context pairs =
  "\nvar context = { " ++ List.intercalate ", " (List.map (\(k,v) -> k ++ ": " ++ v) pairs) ++ " };\n"



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
