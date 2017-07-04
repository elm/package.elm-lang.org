{-# OPTIONS_GHC -Wall #-}
module MoveDocs (move) where

import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.FilePath ((</>))

import qualified Crawl
import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg
import qualified Task
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode



-- MOVE


move :: Pkg.Name -> Pkg.Version -> Task.Transaction ()
move pkg version =
  do  content <- liftIO $ BS.readFile (Crawl.oldDir pkg version </> "documentation.json")
      case Decode.parse (Decode.list Docs.decoder) content of
        Left problem ->
          Task.bail $ "Problem parsing documentation.json:\n" ++ show problem

        Right oldDocs ->
          liftIO $
          do  newDocs <- traverse fixUnmarkedInfix oldDocs
              Encode.write (Crawl.newDir pkg version </> "docs.json") $
                Encode.list Docs.encode newDocs


fixUnmarkedInfix :: Docs.Module -> IO Docs.Module
fixUnmarkedInfix (Docs.Module moduleName docs) =
  let
    fixEntry name entry@(Docs.Entry comment value) =
      case value of
        Docs.Value tipe ->
          if Text.any (\c -> Set.member c symbols) name then
            do  putStrLn $ "  - (" ++ Text.unpack name ++ ") had no assoc/prec"
                return $ Docs.Entry comment (Docs.Infix tipe Docs.Left 9)

          else
            return entry

        Docs.Infix _ _ _ ->
          return entry
  in
    do  values <- Map.traverseWithKey fixEntry (Docs._values docs)
        return $ Docs.Module moduleName (docs {Docs._values = values })


symbols :: Set.Set Char
symbols =
  Set.fromList "!@#$%^&*+-|<>.:"