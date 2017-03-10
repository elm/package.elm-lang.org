{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Sitemap
  ( generate
  )
  where


import Control.Monad (void)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Text.Blaze.Internal ((!))
import qualified Text.Blaze.Internal as Blaze
import qualified Text.Blaze.Renderer.Utf8 as Blaze (renderMarkup)

import Elm.Package (Name, Version)
import qualified Elm.Package as Pkg



-- GENERATE


generate :: Map Name [Version] -> IO ()
generate packages =
  BS.writeFile "sitemap.xml" $
    Blaze.renderMarkup (generateSitemap packages)



-- SITEMAP XML


generateSitemap :: Map Name [Version] -> Blaze.Markup
generateSitemap packages =
  do  Blaze.preEscapedText "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      urlset ! xmlns "http://www.sitemaps.org/schemas/sitemap/0.9" $
        void $ Map.traverseWithKey genPackageUrls packages


genPackageUrls :: Name -> [Version] -> Blaze.Markup
genPackageUrls name versions =
  do  genLatestUrl name
      -- void $ traverse (genExactUrl name) versions


genLatestUrl :: Name -> Blaze.Markup
genLatestUrl name =
  Blaze.customParent "url" $ do
    tag "loc" (url name "latest")
    tag "changefreq" "monthly"
    tag "priority" "0.8"


genExactUrl :: Name -> Version -> Blaze.Markup
genExactUrl name version =
  Blaze.customParent "url" $ do
    tag "loc" (url name (Pkg.versionToText version))
    tag "changefreq" "never"
    tag "priority" "0.3"


url :: Name -> Text -> Text
url name version =
  "http://package.elm-lang.org/packages/"
  <> Pkg.toText name <> "/" <> version



-- XML HELPERS


{-# INLINE urlset #-}
urlset :: Blaze.Markup -> Blaze.Markup
urlset =
  Blaze.customParent (Blaze.textTag "urlset")


xmlns :: Text -> Blaze.Attribute
xmlns value =
  Blaze.customAttribute (Blaze.textTag "xmlns") (Blaze.textValue value)


{-# INLINE tag #-}
tag :: Text -> Text -> Blaze.Markup
tag name content =
  Blaze.customParent (Blaze.textTag name) (Blaze.text content)
