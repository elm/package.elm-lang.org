{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Server (serve) where

import Data.Foldable (asum)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Snap.Core as S
import Snap.Util.FileServe (serveFile, serveDirectory)

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import qualified Json.Encode as Encode

import qualified GitHub
import Memory (Memory)
import qualified Memory
import qualified Memory.History as History
import qualified Package.Register as Register
import qualified Server.Router as Router
import Server.Router (Route, top, s, int, text, custom, (</>), (==>))
import qualified ServeFile



-- SERVE


serve :: GitHub.Token -> Memory -> S.Snap ()
serve token memory =
  asum
    [
      -- NORMAL ROUTES
      Router.serve $ Router.oneOf $
        [ top ==> ServeFile.elm "Elm Packages" "Page.Catalog"
        , s "packages" ==> S.redirect' "/" 301
        , s "packages" </> text </> text </> versionStuff ==> servePackage memory
        , s "all-packages" ==> serveFile "all-packages.json"
        , s "all-packages" </> s "since" </> int ==> serveNewPackages memory
        , s "register" ==> Register.register token memory
        , s "help" </>
            Router.oneOf
              [ s "design-guidelines" ==> ServeFile.elm "Design Guidelines" "Page.DesignGuidelines"
              , s "documentation-format" ==> ServeFile.elm "Documentation Format" "Page.DocumentationFormat"
              , s "docs-preview" ==> ServeFile.previewHtml
              ]
        ]
    ,
      -- STATIC STUFF
      S.route
        [ ("assets", serveDirectory "assets")
        , ("artifacts", serveDirectory "artifacts")
        , ("robots.txt", serveFile "robots.txt")
        , ("sitemap.xml", serveFile "sitemap.xml")
        ]
    ,
      -- NOT FOUND
      do  S.modifyResponse $ S.setResponseStatus 404 "Not Found"
          ServeFile.elm "Not Found" "Page.NotFound"
    ]



-- NEW PACKAGES


serveNewPackages :: Memory -> Int -> S.Snap ()
serveNewPackages memory index =
  do  history <- Memory.getHistory memory
      S.writeBuilder $ Encode.encodeUgly $ Encode.list History.encodeEvent $
        History.since index history



-- PACKAGES


data PkgInfo
  = Overview
  | Docs Vsn (Maybe Text)


data Vsn = Latest | Exactly Pkg.Version


versionStuff :: Route (PkgInfo -> a) a
versionStuff =
  let
    version = custom (either (\_ -> Nothing) Just . Pkg.versionFromText)
    latest = s "latest" ==> Latest
    exactly = version ==> Exactly
    asset = Router.oneOf [ top ==> Nothing, text ==> Just ]
  in
    Router.oneOf
      [ top ==> Overview
      , latest </> asset ==> Docs
      , exactly </> asset ==> Docs
      ]


servePackage :: Memory -> Text -> Text -> PkgInfo -> S.Snap ()
servePackage memory user project info =
  do  let name = Pkg.Name user project

      pkgs <- Memory.getPackages memory

      case Map.lookup name pkgs of
        Nothing ->
          S.pass

        Just versions ->
          case info of
            Overview ->
              ServeFile.overviewHtml name versions

            Docs (Exactly version) asset ->
              if notElem version versions then
                S.pass
              else
                servePackageHelp name version versions asset

            Docs Latest asset ->
              servePackageHelp name (last (List.sort versions)) versions asset


servePackageHelp :: Pkg.Name -> Pkg.Version -> [Pkg.Version] -> Maybe Text -> S.Snap ()
servePackageHelp name version allVersions maybeAsset =
  case maybeAsset of
    Nothing ->
      ServeFile.docsHtml name version Nothing allVersions

    Just "elm.json" ->
      ServeFile.metadata name version "elm.json"

    Just "docs.json" ->
      ServeFile.metadata name version "docs.json"

    Just "README.md" ->
      ServeFile.metadata name version "README.md"

    Just asset ->
      case Module.dehyphenate asset of
        Nothing ->
          S.pass

        Just _ ->
          error "TODO"
