{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Server (serve) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import Snap.Core
  ( Snap, MonadSnap, modifyResponse, pass, redirect', setResponseStatus
  )
import Snap.Util.FileServe
  ( serveDirectoryWith
  , DirectoryConfig(..), fancyDirectoryConfig
  , defaultIndexGenerator, defaultMimeTypes
  )

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import qualified Memory
import Memory (Memory)
import qualified Router
import Router (Route, top, s, int, text, custom, (</>), (==>), oneOf)
import qualified ServeFile



-- SERVE


serve :: Memory -> Snap ()
serve memory =
  Router.serve (route memory)
  <|>
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      (ServeFile.elm "???" "Page.NotFound")


route :: Memory -> Route (Snap () -> b) b
route memory =
  oneOf
    [ top ==> home
    , s "packages" </> packages memory
    , s "all-packages" </> allPackages memory
    , s "assets" ==> serveDirectoryWith directoryConfig "assets"
    , s "artifacts" ==> serveDirectoryWith directoryConfig "artifacts"
    , s "help" </> help
    ]



-- HOME


home :: Snap ()
home =
  ServeFile.elm "Elm Packages" "Page.Catalog"



-- HELP


help :: Route (Snap () -> a) a
help =
  oneOf
    [ s "design-guidelines" ==> ServeFile.elm "Design Guidelines" "Page.DesignGuidelines"
    , s "documentation-format" ==> ServeFile.elm "Documentation Format" "Page.DocumentationFormat"
    , s "docs-preview" ==> ServeFile.pkgPreview
    ]



-- PACKAGES


packages :: Memory -> Route (Snap () -> a) a
packages memory =
  oneOf
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
    latest = s "latest" ==> Latest
    exactly = version ==> Exactly
    asset = oneOf [ top ==> Nothing, text ==> Just ]
  in
    oneOf
      [ top ==> Overview
      , latest </> asset ==> Docs
      , exactly </> asset ==> Docs
      ]


version :: Route (Pkg.Version -> a) a
version =
  custom (either (\_ -> Nothing) Just . Pkg.versionFromText)


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
                servePackageHelp name version asset

            Docs Latest asset ->
              servePackageHelp name (last (List.sort versions)) asset


servePackageHelp :: Pkg.Name -> Pkg.Version -> Maybe Text -> Snap ()
servePackageHelp name version maybeAsset =
  case maybeAsset of
    Nothing ->
      ServeFile.pkgDocs name version Nothing

    Just "docs.json" ->
      error "TODO"

    Just "README.md" ->
      error "TODO"

    Just asset ->
      case Module.dehyphenate asset of
        Nothing ->
          pass

        Just _ ->
          error "TODO"



-- NEW PACKAGES


allPackages :: Memory -> Route (Snap () -> a) a
allPackages memory =
  oneOf
    [ top ==> error "TODO"
    , s "since" </> int ==> serveNewPackages memory
    ]


serveNewPackages :: Memory -> Int -> Snap ()
serveNewPackages memory time =
  do  newPkgs <- Memory.getNewPackages memory time
      return ()



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
