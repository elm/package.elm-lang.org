{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Artifacts
  ( directory
  , js
  , compile
  )
  where


import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Process as Process



-- PATHS


directory :: FilePath
directory =
  "artifacts"


js :: String
js =
  "/" ++ directory ++ "/elm.js"



-- COMPILE


compile :: IO ()
compile =
  do  putStrLn "Compiling artifacts..."

      exitCode <-
        Process.rawSystem "../../compiler/dist/build/elm/elm" ["make" ,"src/frontend/Main.elm"]

      case exitCode of
        Exit.ExitSuccess ->
          do  Dir.createDirectoryIfMissing True "artifacts"
              Dir.renameFile "elm.js" "artifacts/elm.js"

        Exit.ExitFailure _ ->
          do  IO.hPutStrLn IO.stderr "Compilation failed!"
              Exit.exitFailure
