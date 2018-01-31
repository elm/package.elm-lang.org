{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module MoveElmJson
  ( move
  )
  where


import Control.Monad.Trans (liftIO)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import System.FilePath ((</>))

import qualified Crawl
import qualified Elm.Package as Pkg
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Licenses as Licenses
import qualified Json.Encode as Encode
import qualified Task



-- MOVE


move :: Pkg.Name -> Pkg.Version -> Task.Transaction ()
move pkg version =
  do  let oldJson = Crawl.oldDir pkg version </> "elm-package.json"
      content <- liftIO $ BS.readFile oldJson
      case Json.eitherDecode content of
        Left err ->
          Task.bail ("Problem at " ++ oldJson ++ "\n" ++ err)

        Right pkgInfo ->
          do  let newJson = Crawl.newDir pkg version </> "elm.json"
              liftIO $ Encode.writeUgly newJson $
                Project.encode (Project.Pkg pkgInfo)



-- JSON DECODING


instance Json.FromJSON Project.PkgInfo where
  parseJSON value =
    case value of
      Json.Object obj ->
        do  version <- obj .: "version"
            (Txt summmary) <- obj .: "summary"
            repository <- obj .: "repository"
            license <- obj .: "license"
            exposed <- obj .: "exposed-modules"
            deps <- obj .: "dependencies"
            elm <- obj .:? "elm-version"

            return $
              Project.PkgInfo
                (repoToPkgName repository)
                summmary
                (toLicense license)
                version
                (Project.ExposedList exposed)
                deps
                Map.empty
                (toElmVersion elm)



-- UNESCAPED TEXT


newtype Txt = Txt Text.Text


instance Json.FromJSON Txt where
  parseJSON (Json.String txt) =
    return $ Txt $
      Text.replace "\\u003c" "<" (Text.replace "\\u003e" ">" txt)

  parseJSON _ =
    fail "Need a STRING here."



-- VERSION and CONSTRAINT


instance Json.FromJSON Pkg.Version where
  parseJSON (Json.String txt) =
    maybe (fail "bad version") return (Pkg.versionFromText txt)

  parseJSON _ =
    fail "bad version, need STRING"


instance Json.FromJSON Con.Constraint where
  parseJSON (Json.String txt) =
    either fail return (Con.fromText txt)

  parseJSON _ =
    fail "bad constraint, need STRING"


toElmVersion :: Maybe Txt -> Con.Constraint
toElmVersion maybeTxt =
  either error id $ Con.fromText $
    case maybeTxt of
      Nothing ->
        "0.14.0 <= v < 0.15.0"

      Just (Txt text) ->
        text



-- PACKAGE NAMES


instance Json.FromJSON Pkg.Name where
  parseJSON (Json.String txt) =
    return (textToPkgName txt)

  parseJSON _ =
    fail "Problem with package name."


instance Json.FromJSONKey Pkg.Name where
  fromJSONKey =
    Json.FromJSONKeyText textToPkgName


textToPkgName :: Text.Text -> Pkg.Name
textToPkgName rawName =
  case Pkg.fromText rawName of
    Right pkg ->
      pkg

    Left err ->
      case Text.splitOn "/" rawName of
        [user, project] ->
          Pkg.Name user (Text.toLower project)

        _ ->
          error $ "Problem with package name " ++ show rawName ++ " which is " ++ err



-- REPO TO PACKAGE NAME


repoToPkgName :: Text.Text -> Pkg.Name
repoToPkgName repository =
    textToPkgName $
    Text.dropEnd 4 $
    Text.replace "http://github.com/" "" $
    Text.replace "https://github.com/" "" $
      repository



-- TO LICENSE


toLicense :: Text.Text -> Licenses.License
toLicense oldLicense =
  case Licenses.check (Map.findWithDefault oldLicense oldLicense conversions) of
    Right newLicense ->
      newLicense

    Left _ ->
      error $ "Cannot deal with " ++ show oldLicense ++ " license."


conversions :: Map.Map Text.Text Text.Text
conversions =
  Map.fromList
    -- BSD
    [ "BSD" ==> "BSD-3-Clause"
    , "BSD3" ==> "BSD-3-Clause"
    , "BSD3-Clause" ==> "BSD-3-Clause"
    , "BSD 3-Clause" ==> "BSD-3-Clause"
    , "BSD-3-Clause" ==> "BSD-3-Clause"
    , "BSD2" ==> "BSD-2-Clause"

    -- GPL
    , "AGPL-3" ==> "AGPL-3.0"
    , "LGPL-3.0" ==> "LGPL-3.0"
    , "GPL-3.0" ==> "GPL-3.0"
    , "GPLv3" ==> "GPL-3.0"
    , "GPL3" ==> "GPL-3.0"

    -- APACHE
    , "Apache2.0" ==> "Apache-2.0"
    , "Apache 2.0" ==> "Apache-2.0"
    , "Apache-2.0" ==> "Apache-2.0"
    , "Apache license, version 2.0" ==> "Apache-2.0"
    , "Apache License, version 2.0" ==> "Apache-2.0"
    , "Apache License, Version 2.0" ==> "Apache-2.0"

    -- MOZILLA
    , "MPL" ==> "MPL-2.0"
    , "MPL-2.0" ==> "MPL-2.0"
    , "MPL2" ==> "MPL-2.0"
    , "MPLv2" ==> "MPL-2.0"
    , "Mozilla Public License 2.0" ==> "MPL-2.0"
    ]


(==>) :: a -> b -> (a, b)
(==>) =
  (,)

