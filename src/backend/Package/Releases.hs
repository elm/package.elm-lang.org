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
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Time.Clock.POSIX as Time
import qualified System.Directory as Dir
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Json.String as Json
import qualified Package.Path as Path
import qualified Parse.Primitives as P



-- RELEASES


data Release =
  Release
    { _version :: V.Version
    , _time :: Time.POSIXTime
    }
  deriving (Eq, Ord)



-- READ


read :: Pkg.Name -> IO [Release]
read pkg =
  do  json <- BS.readFile (Path.releases pkg)
      case D.fromByteString decoder json of
        Right releases ->
          return releases

        Left _ ->
          do  hPutStr stderr $ "The JSON in " ++ Path.releases pkg ++ " is corrupt."
              exitFailure



-- ADD


add :: Pkg.Name -> V.Version -> Time.POSIXTime -> IO ()
add pkg vsn time =
  do  let path = Path.releases pkg
      exists <- Dir.doesFileExist path
      releases <- if exists then read pkg else return []
      E.write path $ encode $ Release vsn time : releases



-- JSON


encode :: [Release] -> E.Value
encode releases =
  E.object $
    map encodeRelease (List.sort releases)


encodeRelease :: Release -> (Json.String, E.Value)
encodeRelease (Release vsn time) =
  ( Json.fromChars (V.toChars vsn)
  , E.int (floor time)
  )


decoder :: D.Decoder () [Release]
decoder =
  map (uncurry Release) <$>
    D.pairs versionKeyDecoder (fromIntegral <$> D.int)


versionKeyDecoder :: D.KeyDecoder () V.Version
versionKeyDecoder =
  let
    keyParser =
      P.specialize (\_ _ _ -> ()) V.parser
  in
  D.KeyDecoder keyParser (\_ _ -> ())


