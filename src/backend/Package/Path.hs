module Package.Path (releases, directory) where

import System.FilePath ((</>))
import qualified Elm.Package as Pkg


releases :: Pkg.Name -> FilePath
releases pkg =
  "packages" </> Pkg.toFilePath pkg </> "overview.json"


directory :: Pkg.Name -> Pkg.Version -> FilePath
directory name version =
  "packages" </> Pkg.toFilePath name </> Pkg.versionToString version
