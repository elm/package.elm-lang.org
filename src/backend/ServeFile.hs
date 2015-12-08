{-# LANGUAGE OverloadedStrings #-}
module ServeFile (elm, pkgDocs, pkgOverview, pkgPreview) where

import Control.Monad.Trans (liftIO)
import qualified Data.List as List
import Data.Time.Clock.POSIX (getPOSIXTime)
import Snap.Core (Snap, writeBuilder)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import qualified PackageSummary as PkgSummary
import qualified Path



-- TYPICAL PAGES / NO PORTS


elm :: String -> [String] -> Snap ()
elm title elmModuleName =
  makeHtml title elmModuleName (fullscreen elmModuleName [])



-- SPECIAL PAGES


pkgDocs :: Pkg.Name -> Pkg.Version -> Maybe Module.Name -> Snap ()
pkgDocs pkg@(Pkg.Name user project) version maybeName =
  let
    versionString =
      Pkg.versionToString version

    maybeStringName =
      fmap Module.nameToString maybeName

    elmModule =
      ["Page","Package"]

    title =
      maybe "" (++" - ") maybeStringName ++ project ++ " " ++ versionString
  in
    do  allVersions <- getAllVersions pkg

        let context =
              [ ("user", show user)
              , ("project", show project)
              , ("version", show versionString)
              , ("allVersions", show allVersions)
              , ("moduleName", maybe "null" show maybeStringName)
              ]

        makeHtml title elmModule $
          fullscreen elmModule [ ("context", makeObject context) ]


pkgOverview :: Pkg.Name -> Snap ()
pkgOverview pkg@(Pkg.Name user project) =
  let
    elmModule =
      ["Page","PackageOverview"]
  in
    do  allVersions <- getAllVersions pkg

        let context =
              [ ("user", show user)
              , ("project", show project)
              , ("versions", show allVersions)
              ]

        history <-
          liftIO $ readFile $
            "packages" </> user </> project </> "history.json"

        makeHtml (user ++ "/" ++ project) elmModule $
          fullscreen elmModule $
            [ ("context", makeObject context)
            , ("rawHistory", history)
            ]


getAllVersions :: Pkg.Name -> Snap [String]
getAllVersions pkg =
  do  maybeVersions <- liftIO (PkgSummary.readVersionsOf pkg)
      return $ maybe [] (List.map Pkg.versionToString) maybeVersions


pkgPreview :: Snap ()
pkgPreview =
  let
    elmModule =
      ["Page","PreviewDocumentation"]
  in
    makeHtml "Preview your Docs" elmModule $
      fullscreen elmModule [("uploads", "''")]
      ++
      "\nfunction handleFileSelect(evt) {\n\
      \    var reader = new FileReader();\n\
      \    reader.readAsText(evt.target.files[0]);\n\
      \    reader.onload = function(event) {\n\
      \        page.ports.uploads.send(event.target.result);\n\
      \    };\n\
      \}\n\
      \\n\
      \document.getElementById('fileLoader').addEventListener('change', handleFileSelect, false);\n"



-- SKELETON


makeHtml :: String -> [String] -> String -> Snap ()
makeHtml title elmModuleName initializer =
  let
    elmModule =
      Module.Name elmModuleName
  in
  writeBuilder $ Blaze.renderHtmlBuilder $ docTypeHtml $ do
    H.head $ do
      meta ! charset "UTF-8"
      favicon
      H.title (toHtml title)
      googleAnalytics
      link ! rel "stylesheet" ! href (cacheBuster "/assets/highlight/styles/default.css")
      link ! rel "stylesheet" ! href (cacheBuster "/assets/style.css")
      script ! src (cacheBuster "/assets/highlight/highlight.pack.js") $ ""
      script ! src (cacheBuster ("/" ++ Path.artifact elmModule)) $ ""

    body $
      script (preEscapedToMarkup initializer)


fullscreen :: [String] -> [(String, String)] -> String
fullscreen elmModuleName ports =
  "\nvar page = Elm.fullscreen(Elm." ++ Module.nameToString (Module.Name elmModuleName)
  ++ ", " ++ makeObject ports ++ ");\n"


makeObject :: [(String, String)] -> String
makeObject entries =
  "{ " ++ List.intercalate "," (List.map (\(k,v) -> k ++ ": " ++ v) entries) ++ " }"


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


favicon :: H.Html
favicon =
  H.link
    ! A.rel "shortcut icon"
    ! A.size "16x16, 32x32, 48x48, 64x64, 128x128, 256x256"
    ! A.href "/assets/favicon.ico"


cacheBuster :: String -> AttributeValue
cacheBuster url =
  toValue (url ++ "?" ++ uniqueToken)


uniqueToken :: String
uniqueToken =
  unsafePerformIO (show <$> round <$> getPOSIXTime)

