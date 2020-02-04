module Data.ByteString.Validate
  ( isUtf8
  )
  where


import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeIndex)
import Data.Text.Internal.Encoding.Utf8 (validate1, validate2, validate3, validate4)



-- IS UTF-8


isUtf8 :: BS.ByteString -> Bool
isUtf8 bs =
  isValid bs 0 (BS.length bs)


isValid :: BS.ByteString -> Int -> Int -> Bool
isValid bs i len
  | i  >= len                          = True
  |              validate1 w1          = isValid bs (i+1) len
  | i+1 < len && validate2 w1 w2       = isValid bs (i+2) len
  | i+2 < len && validate3 w1 w2 w3    = isValid bs (i+3) len
  | i+3 < len && validate4 w1 w2 w3 w4 = isValid bs (i+4) len
  | otherwise                          = False
  where
    w1 = unsafeIndex bs (i    )
    w2 = unsafeIndex bs (i + 1)
    w3 = unsafeIndex bs (i + 2)
    w4 = unsafeIndex bs (i + 3)
