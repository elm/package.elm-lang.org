{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Artifacts
  ( directory
  , path
  , url
  , compile
  )
  where

import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>))
import System.IO (hPutStrLn, stderr)

import qualified Elm.Compiler.Module as Module
import qualified Elm.Utils as Utils



-- PATHS


directory :: FilePath
directory =
  "artifacts"


path :: Module.Raw -> FilePath
path name =
  directory </> Text.unpack (Module.hyphenate name) <.> "js"


url :: Module.Raw -> FilePath
url name =
  directory ++ "/" ++ Text.unpack (Module.hyphenate name) ++ ".js"



-- COMPILE


compile :: IO ()
compile =
  do  createDirectoryIfMissing True "artifacts"
      result <- runExceptT (mapM runElmMake publicModules)
      case result of
        Right _ ->
          return ()

        Left msg ->
          do  hPutStrLn stderr msg
              exitFailure


runElmMake :: Module.Raw -> ExceptT String IO String
runElmMake name =
  Utils.run "elm-make"
    [ "src" </> "frontend" </> Module.nameToPath name <.> "elm"
    , "--yes"
    , "--output=" ++ path name
    ]


publicModules :: [Module.Raw]
publicModules =
  [ "Page.Catalog"
  , "Page.DesignGuidelines"
  , "Page.DocumentationFormat"
  , "Page.PreviewDocumentation"
  , "Page.NotFound"
  , "Page.Package"
  , "Page.PackageOverview"
  ]
