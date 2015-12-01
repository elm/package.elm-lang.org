{-# LANGUAGE OverloadedStrings #-}
module ServeFile (elm, pkgDocs, pkgOverview, pkgPreview) where

import Control.Monad.Trans (liftIO)
import qualified Data.List as List
import Data.Time.Clock.POSIX (getPOSIXTime)
import Snap.Core (Snap, writeBuilder)
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
  makeHtml title elmModuleName (return Nothing)



-- SPECIAL PAGES


pkgDocs :: Pkg.Name -> Pkg.Version -> Maybe Module.Name -> Snap ()
pkgDocs pkg@(Pkg.Name user project) version maybeName =
  let
    versionString =
      Pkg.versionToString version

    maybeStringName =
      fmap Module.nameToString maybeName

    title =
      maybe "" (++" - ") maybeStringName ++ project ++ " " ++ versionString
  in
    makeHtml title ["Page","Package"] $
      do  allVersions <- getAllVersions pkg
          return $ Just $ makeContext $
            [ ("user", show user)
            , ("project", show project)
            , ("version", show versionString)
            , ("allVersions", show allVersions)
            , ("moduleName", maybe "null" show maybeStringName)
            ]


pkgOverview :: Pkg.Name -> Snap ()
pkgOverview pkg@(Pkg.Name user project) =
  makeHtml (user ++ "/" ++ project) ["Page","PackageOverview"] $
    do  allVersions <- getAllVersions pkg
        return $ Just $ makeContext $
          [ ("user", show user)
          , ("project", show project)
          , ("versions", show allVersions)
          ]


makeContext :: [(String, String)] -> (String, String)
makeContext entries =
  let
    ports =
      "{\n\
      \    context: {"
      ++ List.intercalate "," (List.map (\(k,v) -> "\n        " ++ k ++ ": " ++ v) entries)
      ++ "\n    }\n}"
  in
    (ports, "")


getAllVersions :: Pkg.Name -> Snap [String]
getAllVersions pkg =
  do  maybeVersions <- liftIO (PkgSummary.readVersionsOf pkg)
      return $ maybe [] (List.map Pkg.versionToString) maybeVersions


pkgPreview :: Snap ()
pkgPreview =
  makeHtml "Preview your Docs" ["Page","PreviewDocumentation"] $
    return $ Just $ (,) "{ uploads: '' }" $
      "function handleFileSelect(evt) {\n\
      \    var reader = new FileReader();\n\
      \    reader.readAsText(evt.target.files[0]);\n\
      \    reader.onload = function(event) {\n\
      \        page.ports.uploads.send(event.target.result);\n\
      \    };\n\
      \}\n\
      \\n\
      \document.getElementById('fileLoader').addEventListener('change', handleFileSelect, false);\n"



-- SKELETON


makeHtml :: String -> [String] -> Snap (Maybe (String, String)) -> Snap ()
makeHtml title elmModuleName makePorts =
  let
    elmModule =
      Module.Name elmModuleName
  in
  do  maybePorts <- makePorts
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
          script $ preEscapedToMarkup $
            case maybePorts of
              Nothing ->
                "\nElm.fullscreen(Elm." ++ Module.nameToString elmModule ++ ")\n"

              Just (ports, postScript) ->
                "\nvar page = Elm.fullscreen(Elm."
                ++ Module.nameToString elmModule
                ++ ", "
                ++ ports
                ++ ");\n\n"
                ++ postScript


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
