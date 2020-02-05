{-# LANGUAGE OverloadedStrings #-}
module ServeGzip
  ( serveGzippedFile
  )
  where


import qualified Data.Binary.Builder as B
import qualified Data.ByteString as BS
import Snap.Core
  ( Snap, finishWith, getHeader, getRequest, getResponse
  , modifyResponse, rqURI, sendFile
  , setContentType, setHeader, setResponseBody, setResponseStatus
  , writeBuilder
  )
import qualified System.IO.Streams.Core as S
import qualified System.IO.Streams.File as SF
import qualified System.IO.Streams.Zlib as SZ



-- SERVE GZIP
--
-- Serve files like:
--
--   README.md.gz
--   docs.json.gz
--   elm.json.gz
--
-- They are stored on disk compressed to save space, and we only need to do
-- additional computational work if people are making requests that do not
-- have an "Accept-Encoding: gzip" header.


serveGzippedFile :: BS.ByteString -> FilePath -> Snap ()
serveGzippedFile mimeType filePath =
  do  encoding <- getAcceptableEncoding
      case encoding of
        Gzip ->
          do  sendFile filePath
              finishWith
                . setHeader "Content-Encoding" "gzip"
                . setHeader "Vary" "Accept-Encoding"
                . setContentType mimeType
                =<< getResponse

        Identity ->
          do  userAgent <- detectUserAgent
              case userAgent of
                Curl ->
                  do  modifyResponse $ setResponseStatus 406 "Bad Accept-Encoding"
                      writeBuilder . toCurlError . rqURI =<< getRequest

                Wget ->
                  do  modifyResponse $ setResponseStatus 406 "Bad Accept-Encoding"
                      writeBuilder . toWgetError . rqURI =<< getRequest

                Browser ->
                  finishWith
                    . setHeader "Content-Encoding" "identity"
                    . setHeader "Vary" "Accept-Encoding"
                    . setContentType mimeType
                    . setResponseBody (gunzipIntoOutput filePath)
                    =<< getResponse


gunzipIntoOutput :: FilePath -> S.OutputStream B.Builder -> IO (S.OutputStream B.Builder)
gunzipIntoOutput filePath output =
  SF.withFileAsInput filePath $ \input ->
    do  unzippedInput <-  SZ.gunzip input
        connect unzippedInput output
        return output


{-# INLINE connect #-}
connect :: S.InputStream BS.ByteString -> S.OutputStream B.Builder -> IO ()
connect input output =
    loop
  where
    loop =
      do  maybeBytes <- S.read input
          case maybeBytes of
            Nothing -> S.write Nothing output
            Just bs ->
              do  S.write (Just (B.fromByteString bs)) output
                  loop



-- GET ENCODING


data Encoding
  = Gzip
  | Identity


getAcceptableEncoding :: Snap Encoding
getAcceptableEncoding =
  do  maybeHeader <- getHeader "Accept-Encoding" <$> getRequest
      case maybeHeader of
        Nothing ->
          return Identity

        Just header ->
          if BS.isInfixOf "gzip" header
          then return Gzip
          else return Identity



-- DETECT USER AGENT


data UserAgent
  = Browser
  | Curl
  | Wget


detectUserAgent :: Snap UserAgent
detectUserAgent =
  do  maybeHeader <- getHeader "User-Agent" <$> getRequest
      case maybeHeader of
        Nothing ->
          return Browser

        Just header
          | BS.isInfixOf "curl" header -> return Curl
          | BS.isInfixOf "Wget" header -> return Wget
          | otherwise                  -> return Browser


toCurlError :: BS.ByteString -> B.Builder
toCurlError uri =
  "-- 406 -- MISSING accept-encoding HEADER --------------\n\
  \\n\
  \Add the --compressed flag to help reduce bandwidth costs!\n\
  \\n\
  \    curl --compressed https://package.elm-lang.org" <> B.fromByteString uri <> "\n\
  \\n\
  \If that does not work for some reason, you can try the following:\n\
  \\n\
  \    curl -sH 'accept-encoding: gzip' https://package.elm-lang.org" <> B.fromByteString uri <> " | gunzip\n\
  \\n\
  \Server costs are paid by individual community members, not some big company, so\n\
  \thank you for helping out like this!\n\n"


toWgetError :: BS.ByteString -> B.Builder
toWgetError uri =
  "-- 406 -- MISSING accept-encoding HEADER --------------\n\
  \\n\
  \Please download the gzipped version to help reduce bandwidth costs!\n\
  \\n\
  \    wget --header=\"accept-encoding: gzip\" https://package.elm-lang.org" <> B.fromByteString uri <> " | gunzip\n\
  \\n\
  \Server costs are paid by individual community members, not some big company, so\n\
  \thank you for helping out like this!\n\n"
