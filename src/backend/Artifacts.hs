{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Artifacts
  ( Artifacts(..)
  , init
  , serve
  )
  where


import Prelude hiding (init)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Snap.Core as S
import qualified System.Process as Process

import qualified Helpers
import qualified ServeGzip



-- ARTIFACTS


data Artifacts =
  Artifacts
    { _elmHash :: BS.ByteString
    }



-- INIT


init :: IO Artifacts
init =
  Helpers.report "Artifacts" $
  do  elmHash <- Process.readProcess "bash" ["build.sh"] ""
      return (Artifacts (BSC.pack elmHash))



-- SERVE


serve :: Artifacts -> BS.ByteString -> S.Snap ()
serve (Artifacts elmHash) hash
  | hash == elmHash = serveGzippedArtifact "application/javascript" elmHash
  | otherwise       =
      do  S.modifyResponse $ S.setResponseStatus 404 "Not Found"
          S.writeBuilder "404 Not Found"
          S.finishWith =<< S.getResponse


serveGzippedArtifact :: BS.ByteString -> BS.ByteString -> S.Snap ()
serveGzippedArtifact mimeType hash =
  do  setETagOr304 hash
      S.modifyResponse $ S.setHeader "cache-control" "immutable"
      ServeGzip.serveGzippedFile mimeType ("artifacts/" ++ BSC.unpack hash)



-- SET ETAG OR 304


setETagOr304 :: BS.ByteString -> S.Snap ()
setETagOr304 hash =
  do  let etag = BS.concat ["\"", hash, "\""]
      maybeHeader <- S.getsRequest (S.getHeader "if-none-match")
      case maybeHeader of
        Nothing ->
          S.modifyResponse $ S.setHeader "ETag" etag

        Just clientETag ->
          if etag == clientETag
          then S.finishWith $ S.setResponseCode 304 S.emptyResponse
          else S.modifyResponse $ S.setHeader "ETag" etag
