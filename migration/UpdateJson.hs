{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module UpdateJson (update) where

import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import System.FilePath ((</>))

import qualified Crawl
import qualified Elm.Package as Pkg
import qualified Task



-- MAIN


update :: [Crawl.Package] -> Task.Task ()
update packages =
  mapM_ updatePackage packages


updatePackage :: Crawl.Package -> Task.Task ()
updatePackage (Crawl.Package pkg vsns) =
  mapM_ (updateVersion pkg) vsns


updateVersion :: Pkg.Name -> Pkg.Version -> Task.Task ()
updateVersion pkg version =
  do  let oldJson = Crawl.oldDir pkg version </> "elm-package.json"
      content <- liftIO $ BS.readFile oldJson
      case Json.eitherDecode content of
        Left err ->
          throwError $ "Problem at " ++ oldJson ++ "\n" ++ err

        Right (Project object) ->
          do  let newJson = Crawl.newDir pkg version </> "elm.json"
              liftIO $ BS.writeFile newJson (Json.encode object)


data Project = Project Json.Object


instance Json.FromJSON Project where
  parseJSON =
    Json.withObject "Project" $ \object ->
      do
          (Json.String license) <- object .: "license"
          (Json.String repo) <- object .: "repository"

          case Map.lookup license conversions of
            Nothing ->
              fail $ "cannot find " ++ Text.unpack license ++ " in license conversions table"

            Just spdxLicense ->
              return $ Project $
                HashMap.insert "type" (Json.String "package") $
                HashMap.delete "repository" $
                HashMap.insert "name" (Json.String (toName repo)) $
                HashMap.insert "license" (Json.String spdxLicense) $
                HashMap.insert "test-dependencies" (Json.Object HashMap.empty) $
                  object


toName :: Text.Text -> Text.Text
toName repository =
  Text.dropEnd 4 $
  Text.replace "http://github.com/" "" $
  Text.replace "https://github.com/" "" $
    repository



-- LICENSE CONVERTER


conversions :: Map.Map Text.Text Text.Text
conversions =
  Map.fromList
--    [ "" ==> "BSD-3-Clause"

    -- BSD
    [ "BSD3" ==> "BSD-3-Clause"
    , "BSD3-Clause" ==> "BSD-3-Clause"
    , "BSD 3-Clause" ==> "BSD-3-Clause"
    , "BSD-3-Clause" ==> "BSD-3-Clause"
    , "BSD2" ==> "BSD-2-Clause"

    -- CORRECT ONES
    , "MIT" ==> "MIT"
    , "ISC" ==> "ISC"

    -- GPL
    , "AGPL-3" ==> "AGPL-3.0"
    , "GPLv3" ==> "GPL-3.0"

    -- APACHE
    , "Apache2.0" ==> "Apache-2.0"
    , "Apache 2.0" ==> "Apache-2.0"
    , "Apache-2.0" ==> "Apache-2.0"
    , "Apache License, version 2.0" ==> "Apache-2.0"
    , "Apache License, Version 2.0" ==> "Apache-2.0"

    -- MOZILLA
    , "MPL" ==> "MPL-2.0"
    , "MPL-2.0" ==> "MPL-2.0"
    , "MPL2" ==> "MPL-2.0"
    , "MPLv2" ==> "MPL-2.0"
    , "Mozilla Public License 2.0" ==> "MPL-2.0"

    -- NOT OSI
    , "CC0" ==> "CC0-1.0"
    , "WTFPL" ==> "WTFPL"
    , "WORDNET" ==> "WORDNET"
    ]


(==>) :: a -> b -> (a, b)
(==>) =
  (,)

