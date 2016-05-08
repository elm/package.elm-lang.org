{-# LANGUAGE OverloadedStrings #-}
module PackageSummary
  ( Summary(..)
  , allPackages, allPackages16, allPackages15
  , add
  , readVersionsOf
  )
  where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.List as List
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified System.Directory as Dir
import System.IO

import qualified Elm.Package as Pkg
import qualified Elm.Package.Description as Desc



data Summary = Summary
    { name     :: Pkg.Name
    , summary  :: String
    , versions :: [Pkg.Version]
    }


allPackages :: String
allPackages =
    "all-packages.json"


allPackages16 :: String
allPackages16 =
    "all-packages-16.json"


allPackages15 :: String
allPackages15 =
    "all-packages-15.json"



-- ADD A SUMMARY


add :: Desc.Description -> IO ()
add desc =
  do  summaries <- readAllSummaries
      LBS.writeFile allPackages (jsonEncode (insert summary summaries))
  where
    summary =
        Summary (Desc.name desc) (Desc.summary desc) [Desc.version desc]


insert :: Summary -> [Summary] -> [Summary]
insert summary summaries =
    case summaries of
      [] -> [summary]

      currentSummary : rest ->
          case compare (name summary) (name currentSummary) of
            GT ->
                currentSummary : insert summary rest

            LT ->
                summary : summaries

            EQ ->
                let vs = versions summary ++ versions currentSummary
                in
                    summary { versions = vs } : rest



-- READING SUMMARIES


readAllSummaries :: IO [Summary]
readAllSummaries =
  do  exists <- Dir.doesFileExist allPackages16
      case exists of
        False ->
          do  LBS.writeFile allPackages16 (jsonEncode ([] :: [Summary]))
              return []

        True ->
          withBinaryFile allPackages16 ReadMode $ \handle ->
              do  json <- LBS.hGetContents handle
                  case Json.eitherDecode json of
                    Left msg ->
                        error msg

                    Right summaries ->
                        return summaries


readVersionsOf :: Pkg.Name -> IO (Maybe [Pkg.Version])
readVersionsOf packageName =
  do  summaries <- readAllSummaries
      let maybeSummary =
              List.find (\summary -> packageName == name summary) summaries
      return (fmap versions maybeSummary)



-- JSON


jsonEncode :: Json.ToJSON a => a -> LBS.ByteString
jsonEncode value =
    Json.encodePretty' config value


config :: Json.Config
config =
    Json.defConfig {
        Json.confCompare = Json.keyOrder [ "name", "summary", "versions" ]
    }


instance Json.ToJSON Summary where
    toJSON (Summary name summary versions) =
        Json.object
        [ "name" .= name
        , "summary" .= summary
        , "versions" .= versions
        ]


instance Json.FromJSON Summary where
    parseJSON json =
        case json of
          Json.Object v ->
              Summary
                  <$> v .: "name"
                  <*> v .: "summary"
                  <*> v .: "versions"

          _ ->
              fail $
                  "Could not parse a Summary from value "
                  ++ LBS.unpack (Json.encode json)
