{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module ServeFile
  ( misc
  , project
  , version
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Utf8 as Utf8
import Data.Time.Clock.POSIX (getPOSIXTime)
import Snap.Core (Snap, writeBuilder)
import System.IO.Unsafe (unsafePerformIO)
import Text.RawString.QQ (r)

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V

import qualified Artifacts as A



-- TYPICAL PAGES / NO PORTS


misc :: A.Artifacts -> B.Builder -> Snap ()
misc artifacts title =
  makeHtml artifacts title mempty



-- PROJECT


project :: A.Artifacts -> Pkg.Name -> Snap ()
project artifacts pkg =
  makeHtml artifacts (B.stringUtf8 (Pkg.toChars pkg)) mempty



-- VERSION


version :: A.Artifacts -> Pkg.Name -> V.Version -> Maybe ModuleName.Raw -> Snap ()
version artifacts pkg vsn maybeName =
  makeHtml artifacts
    (B.stringUtf8 (toTitle pkg vsn maybeName))
    (makeCanonicalLink pkg maybeName)


toTitle :: Pkg.Name -> V.Version -> Maybe ModuleName.Raw -> String
toTitle (Pkg.Name _ proj) vsn maybeName =
  case maybeName of
    Nothing ->
      Utf8.toChars proj ++ " " ++ V.toChars vsn

    Just name ->
      ModuleName.toChars name ++ " - " ++
      Utf8.toChars proj ++ " " ++ V.toChars vsn




-- CANONICAL LINKS


makeCanonicalLink :: Pkg.Name -> Maybe ModuleName.Raw -> B.Builder
makeCanonicalLink pkg maybeName =
  [r|<link rel="canonical" href="/packages/|]
    <> toCanonicalPackage pkg
    <> [r|/latest/|]
    <> maybe "" (B.stringUtf8 . ModuleName.toChars) maybeName
    <> [r|">|]


toCanonicalPackage :: Pkg.Name -> B.Builder
toCanonicalPackage pkg =
  let
    url = Pkg.toUrl pkg
  in
  B.stringUtf8 (Map.findWithDefault url url renames)


renames :: Map.Map String String
renames =
  Map.fromList
    [ "evancz/elm-effects" ==> "elm/core"
    , "evancz/elm-html" ==> "elm/html"
    , "evancz/elm-http" ==> "elm/http"
    , "evancz/elm-svg" ==> "elm/svg"
    , "evancz/start-app" ==> "elm/html"
    , "evancz/virtual-dom" ==> "elm/virtual-dom"

    , "elm-lang/animation-frame" ==> "elm/browser"
    , "elm-lang/core" ==> "elm/core"
    , "elm-lang/html" ==> "elm/html"
    , "elm-lang/http" ==> "elm/http"
    , "elm-lang/svg" ==> "elm/svg"
    , "elm-lang/virtual-dom" ==> "elm/virtual-dom"

    , "elm-community/elm-list-extra" ==> "elm-community/list-extra"
    , "elm-community/elm-linear-algebra" ==> "elm-community/linear-algebra"
    , "elm-community/elm-lazy-list" ==> "elm-community/lazy-list"
    , "elm-community/elm-json-extra" ==> "elm-community/json-extra"
    ]


(==>) :: a -> b -> (a, b)
(==>) =
  (,)



-- SKELETON


makeHtml :: A.Artifacts -> B.Builder -> B.Builder -> Snap ()
makeHtml artifacts title canonicalLink =
  writeBuilder $
    [r|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <link rel="shortcut icon" size="16x16, 32x32, 48x48, 64x64, 128x128, 256x256" href="/assets/favicon.ico">
  <title>|] <> title <> [r|</title>|] <> canonicalLink <> [r|
  <link rel="stylesheet" href="/assets/highlight/styles/default.css?|] <> uniqueToken <> [r|">
  <link rel="stylesheet" href="/assets/style.css?|] <> uniqueToken <> [r|">
  <script src="/assets/highlight/highlight.pack.js?|] <> uniqueToken <> [r|"></script>
  <script src="/artifacts/|] <> B.byteString (A._elmHash artifacts) <> [r|"></script>
</head>
<body>
<script>
Elm.Main.init();
</script>
</body>
</html>|]


uniqueToken :: B.Builder
uniqueToken =
  unsafePerformIO $
    do  time <- getPOSIXTime
        return $ B.string7 $ show (floor time :: Integer)
