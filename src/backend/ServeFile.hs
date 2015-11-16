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


filler :: String -> Module.Name -> Snap ()
filler title name =
    writeBuilder $
    Blaze.renderHtmlBuilder $
    docTypeHtml $ do
      H.head $ do
        meta ! charset "UTF-8"
        favicon
        H.title (toHtml title)
        googleAnalytics
        link ! rel "stylesheet" ! href "/assets/highlight/styles/default.css"
        link ! rel "stylesheet" ! href "/assets/style.css"
        script ! src "/assets/highlight/highlight.pack.js" $ ""
        script ! src (toValue ("/" ++ Path.artifact name)) $ ""

      body $
        script $ preEscapedToMarkup $
          "Elm.fullscreen(Elm." ++ Module.nameToString name ++ ")"


pkgDocs :: Pkg.Name -> Pkg.Version -> Maybe Module.Name -> Snap ()
pkgDocs pkg@(Pkg.Name user name) version maybeName =
  let
    versionString =
      Pkg.versionToString version

    maybeStringName =
      fmap Module.nameToString maybeName
  in
  do  maybeVersions <- liftIO (PkgSummary.readVersionsOf pkg)
      let versionList =
            maybe [] (List.map Pkg.versionToString) maybeVersions

      writeBuilder $
        Blaze.renderHtmlBuilder $
        docTypeHtml $ do
          H.head $ do
            meta ! charset "UTF-8"
            favicon
            H.title (toHtml (maybe "" (++" - ") maybeStringName ++ name ++ " " ++ versionString))
            googleAnalytics
            link ! rel "stylesheet" ! href "/assets/highlight/styles/default.css"
            link ! rel "stylesheet" ! href "/assets/style.css"
            script ! src "/assets/highlight/highlight.pack.js" $ ""
            script ! src "/artifacts/Page-Package.js" $ ""

          body $ script $ preEscapedToMarkup $
              context
                [ ("user", show user)
                , ("project", show name)
                , ("version", show versionString)
                , ("allVersions", show versionList)
                , ("moduleName", maybe "null" show maybeStringName)
                ]
              ++
                "var page = Elm.fullscreen(Elm.Page.Package, { context: context });\n"


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
