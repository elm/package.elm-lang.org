{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Package.Releases
  ( Release(..)
  , read
  , add
  , encode
  )
  where


import Prelude hiding (read)
import Control.Monad (forM)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HashMap
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as Time
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

import qualified Elm.Package as Pkg
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
import qualified Package.Path as Path



-- RELEASES


data Release =
  Release
    { _version :: Pkg.Version
    , _time :: Time.POSIXTime
    }



-- READ


read :: Pkg.Name -> IO [Release]
read name =
  do  json <- BS.readFile (Path.releases name)
      case Decode.parse releasesDecoder json of
        Right releases ->
          return releases

        Left _ ->
          do  hPutStr stderr $ "The JSON in " ++ Path.releases name ++ " is corrupt."
              exitFailure



-- ADD


add :: Pkg.Name -> Pkg.Version -> Time.POSIXTime -> IO ()
add name version time =
  do  let path = Path.releases name
      exists <- doesFileExist path
      releases <- if exists then read name else return []
      Encode.write path $ encode $ Release version time : releases



-- JSON


encode :: [Release] -> Encode.Value
encode releases =
  Encode.object $ flip map releases $ \(Release version time) ->
    ( Pkg.versionToString version, Encode.int (floor time) )


releasesDecoder :: Decode.Decoder [Release]
releasesDecoder =
  do  pairs <- HashMap.toList <$> Decode.dict Decode.int
      forM pairs $ \(vsn, int) ->
        case Pkg.versionFromText vsn of
          Left _ ->
            Decode.fail $ Text.unpack vsn <> " is not a valid version"

          Right version ->
            Decode.succeed (Release version (fromIntegral int))
