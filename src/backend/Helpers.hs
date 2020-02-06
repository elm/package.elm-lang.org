module Helpers
  ( report
  )
  where


import System.IO (hFlush, hPutStr, hPutStrLn, stdout)



-- REPORT


report :: String -> IO a -> IO a
report name work =
  do  hPutStr stdout $ name ++ " ... "
      hFlush stdout
      a <- work
      hPutStrLn stdout "DONE"
      return a
