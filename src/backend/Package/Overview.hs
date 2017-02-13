{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Package.Overview
  ( Overview(..)
  , Info(..)
  , read
  , add
  )
  where


import Prelude hiding (read)
import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HashMap
import Data.Monoid ((<>))
import qualified Data.Time.Clock as Time
import qualified Data.Text as Text
import Data.Text (Text)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

import qualified Elm.Package as Pkg



-- SUMMARY


newtype Overview = Overview [Info]


data Info =
  Info
    { _version :: Pkg.Version
    , _date :: Time.NominalDiffTime
    }



-- TO JSON


instance Json.ToJSON Overview where
  toJSON (Overview infos) =
    Json.object (map toPair infos)


toPair :: Info -> Json.Pair
toPair (Info version date) =
  Pkg.versionToText version .= date



-- FROM JSON


instance Json.FromJSON Overview where
  parseJSON =
    Json.withObject "Overview" $ \hash ->
      Overview <$> traverse fromPair (HashMap.toList hash)


fromPair :: (Text, Json.Value) -> Json.Parser Info
fromPair (rawVersion, rawTime) =
  do  time <- Json.parseJSON rawTime
      case Pkg.versionFromText rawVersion of
        Left _ ->
          fail $ Text.unpack $
            "`" <> rawVersion <> "` is not a valid version"

        Right version ->
          return $ Info version time



-- READ


read :: FilePath -> IO Overview
read filePath =
  do  json <- BS.readFile filePath
      case Json.decode' json of
        Just overview ->
          return overview

        Nothing ->
          do  hPutStr stderr $ "The JSON in " ++ filePath ++ " is corrupt."
              exitFailure



-- ADD


add :: FilePath -> Pkg.Version -> Time.NominalDiffTime -> IO ()
add filePath version date =
  do  exists <- doesFileExist filePath

      (Overview infos) <-
        if exists
          then read filePath
          else return (Overview [])

      BS.writeFile filePath $ Json.encode $ Overview $
        Info version date : infos
