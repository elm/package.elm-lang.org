{-# OPTIONS_GHC -Wall #-}
module Sitemap
  ( generate
  )
  where


import Control.Monad (void)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Map (Map)
import Text.Blaze.Internal ((!))
import qualified Text.Blaze.Internal as Blaze
import qualified Text.Blaze.Renderer.Utf8 as Blaze (renderMarkup)

import qualified Elm.Package as Pkg
import qualified Elm.Version as V



-- GENERATE


generate :: Map Pkg.Name a -> IO ()
generate packages =
  BS.writeFile "sitemap.xml" $
    Blaze.renderMarkup (generateSitemap packages)



-- SITEMAP XML


generateSitemap :: Map Pkg.Name a -> Blaze.Markup
generateSitemap packages =
  do  Blaze.preEscapedString "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      urlset ! xmlns "http://www.sitemaps.org/schemas/sitemap/0.9" $
        void $ Map.traverseWithKey genPackageUrls packages


genPackageUrls :: Pkg.Name -> a -> Blaze.Markup
genPackageUrls name _ =
  do  genLatestUrl name
      -- void $ traverse (genExactUrl name) (getVersions summary)


genLatestUrl :: Pkg.Name -> Blaze.Markup
genLatestUrl pkg =
  url $
    do  loc pkg "latest"
        changefreq "monthly"
        priority "0.8"


_genExactUrl :: Pkg.Name -> V.Version -> Blaze.Markup
_genExactUrl pkg version =
  url $
    do  loc pkg (V.toChars version)
        changefreq "never"
        priority "0.3"



-- XML HELPERS


{-# INLINE urlset #-}
urlset :: Blaze.Markup -> Blaze.Markup
urlset =
  Blaze.customParent (Blaze.stringTag "urlset")


xmlns :: String -> Blaze.Attribute
xmlns value =
  Blaze.customAttribute (Blaze.stringTag "xmlns") (Blaze.stringValue value)


url :: Blaze.Markup -> Blaze.Markup
url =
  Blaze.customParent (Blaze.stringTag "url")


loc :: Pkg.Name -> String -> Blaze.Markup
loc pkg vsn =
  Blaze.customParent (Blaze.stringTag "loc") $ Blaze.string $
    "https://package.elm-lang.org/packages/" ++ Pkg.toChars pkg ++ "/" ++ vsn


changefreq :: String -> Blaze.Markup
changefreq value =
  Blaze.customParent (Blaze.stringTag "changefreq") (Blaze.string value)


priority :: String -> Blaze.Markup
priority value =
  Blaze.customParent (Blaze.stringTag "priority") (Blaze.string value)
