module Server.Error (string, bytestring) where

import qualified Data.ByteString.Char8 as BS
import Snap.Core


string :: Int -> String -> Snap a
string code problem =
  bytestring code (BS.pack problem)


bytestring :: Int -> BS.ByteString -> Snap a
bytestring code problem =
  do  modifyResponse (setResponseCode code)
      writeBS problem
      finishWith =<< getResponse
