module Path where

import System.FilePath ((</>), (<.>))

import Elm.Compiler.Module as Module


artifactDirectory :: FilePath
artifactDirectory =
    "artifacts"


artifact :: Module.Name -> FilePath
artifact name =
    artifactDirectory </> Module.hyphenate name <.> "js"