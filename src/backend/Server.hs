{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Server (serve) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Snap.Core as Snap
import Snap.Core
  ( Snap, MonadSnap, modifyResponse, pass, redirect'
  , setResponseStatus, writeBuilder
  )
import Snap.Util.FileServe
  ( serveFile, serveDirectoryWith
  , DirectoryConfig(..), fancyDirectoryConfig
  , defaultIndexGenerator, defaultMimeTypes
  )

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import qualified Json.Encode as Encode

import Memory (Memory)
import qualified Memory
import qualified Memory.History as History
import qualified Router
import Router (Route, top, s, int, text, custom, (</>), (==>))
import qualified ServeFile



-- SERVE


serve :: Memory -> Snap ()
serve memory =
  Router.serve (route memory)
  <|> robots
  <|> notFound


route :: Memory -> Route (Snap () -> b) b
route memory =
  Router.oneOf
    [ top ==> home
    , s "packages" </> packages memory
    , s "all-packages" </> allPackages memory
    , s "assets" ==> serveDirectoryWith directoryConfig "assets"
    , s "artifacts" ==> serveDirectoryWith directoryConfig "artifacts"
    , s "help" </> help
    ]


robots :: Snap ()
robots =
  Snap.route
    [ ("robots.txt", serveFile "robots.txt")
    , ("sitemap.xml", serveFile "sitemap.xml")
    ]


notFound :: Snap ()
notFound =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      (ServeFile.elm "Not Found" "Page.NotFound")


-- HOME


home :: Snap ()
home =
  ServeFile.elm "Elm Packages" "Page.Catalog"



-- HELP


help :: Route (Snap () -> a) a
help =
  Router.oneOf
    [ s "design-guidelines" ==> ServeFile.elm "Design Guidelines" "Page.DesignGuidelines"
    , s "documentation-format" ==> ServeFile.elm "Documentation Format" "Page.DocumentationFormat"
    , s "docs-preview" ==> ServeFile.pkgPreview
    ]



-- PACKAGES


packages :: Memory -> Route (Snap () -> a) a
packages memory =
  Router.oneOf
    [ top ==> redirect' "/" 301
    , text </> text </> versionStuff ==> servePackage memory
    ]


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


servePackage :: Memory -> Text -> Text -> PkgInfo -> Snap ()
servePackage memory user project info =
  do  let name = Pkg.Name user project

      pkgs <- Memory.getPackages memory

      case Map.lookup name pkgs of
        Nothing ->
          pass

        Just versions ->
          case info of
            Overview ->
              ServeFile.pkgOverview name versions

            Docs (Exactly version) asset ->
              if notElem version versions then
                pass
              else
                servePackageHelp name version versions asset

            Docs Latest asset ->
              servePackageHelp name (last (List.sort versions)) versions asset


servePackageHelp :: Pkg.Name -> Pkg.Version -> [Pkg.Version] -> Maybe Text -> Snap ()
servePackageHelp name version allVersions maybeAsset =
  case maybeAsset of
    Nothing ->
      ServeFile.pkgDocs name version Nothing allVersions

    Just "elm.json" ->
      ServeFile.static name version "elm.json"

    Just "docs.json" ->
      ServeFile.static name version "docs.json"

    Just "README.md" ->
      ServeFile.static name version "README.md"

    Just asset ->
      case Module.dehyphenate asset of
        Nothing ->
          pass

        Just _ ->
          error "TODO"



-- NEW PACKAGES


allPackages :: Memory -> Route (Snap () -> a) a
allPackages memory =
  Router.oneOf
    [ top ==> serveFile "all-packages.json"
    , s "since" </> int ==> serveNewPackages memory
    ]


serveNewPackages :: Memory -> Int -> Snap ()
serveNewPackages memory index =
  do  history <- Memory.getHistory memory
      writeBuilder $ Encode.encode $ Encode.list History.encodeEvent $
        History.since index history



-- DIRECTORY CONFIG


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
  fancyDirectoryConfig
    { indexGenerator =
        defaultIndexGenerator defaultMimeTypes indexStyle
    , mimeTypes =
        HashMap.insert ".elm" "text/plain" $
        HashMap.insert ".ico" "image/x-icon" $
          defaultMimeTypes
    }


indexStyle :: BS.ByteString
indexStyle =
    "body { margin:0; font-family:sans-serif; background:rgb(245,245,245);\
    \       font-family: calibri, verdana, helvetica, arial; }\
    \div.header { padding: 40px 50px; font-size: 24px; }\
    \div.content { padding: 0 40px }\
    \div.footer { display:none; }\
    \table { width:100%; border-collapse:collapse; }\
    \td { padding: 6px 10px; }\
    \tr:nth-child(odd) { background:rgb(216,221,225); }\
    \td { font-family:monospace }\
    \th { background:rgb(90,99,120); color:white; text-align:left;\
    \     padding:10px; font-weight:normal; }"
