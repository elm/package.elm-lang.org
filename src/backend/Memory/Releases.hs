{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Memory.Releases
  ( Release(..)
  , read
  , add
  )
  where


import Prelude hiding (read)
import Control.Monad (forM)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HashMap
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStr, stderr)

import qualified Elm.Package as Pkg
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode



-- RELEASES


data Release =
  Release
    { _version :: Pkg.Version
    , _time :: Time.NominalDiffTime
    }


toPath :: Pkg.Name -> FilePath
toPath pkg =
  "packages" </> Pkg.toFilePath pkg </> "releases.json"



-- READ


read :: Pkg.Name -> IO [Release]
read pkg =
  do  let path = toPath pkg
      json <- BS.readFile path
      case Decode.parse releaseDecoder json of
        Right overview ->
          return overview

        Left _ ->
          do  hPutStr stderr $ "The JSON in " ++ path ++ " is corrupt."
              exitFailure



-- ADD


add :: Pkg.Name -> Pkg.Version -> Time.NominalDiffTime -> IO ()
add pkg version time =
  do  let path = toPath pkg
      exists <- doesFileExist path
      releases <- if exists then read pkg else return []
      Encode.write path $ encodeRelease $ Release version time : releases



-- JSON


encodeRelease :: [Release] -> Encode.Value
encodeRelease releases =
  Encode.object $ flip map releases $ \(Release version time) ->
    ( Pkg.versionToString version, Encode.int (floor time) )


releaseDecoder :: Decode.Decoder [Release]
releaseDecoder =
  do  pairs <- HashMap.toList <$> Decode.dict Decode.int
      forM pairs $ \(vsn, int) ->
        case Pkg.versionFromText vsn of
          Left _ ->
            Decode.fail $ Text.unpack vsn <> " is not a valid version"

          Right version ->
            Decode.succeed (Release version (fromIntegral int))
