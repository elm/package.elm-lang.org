{-# OPTIONS_GHC -Wall #-}
module Memory
  ( Memory
  , Summary(..)
  , init
  , getPackages
  , getHistory
  , addPackage
  )
  where


import Prelude hiding (init)
import Control.Concurrent
  ( forkIO, Chan, newChan, readChan, writeChan
  , MVar, newMVar, putMVar, readMVar, takeMVar
  )
import Control.Monad (forever, join)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Snap.Core (Snap)
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Package as Pkg
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Licenses as License
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode

import Memory.History (History)
import qualified Memory.History as History
import qualified Sitemap



-- MEMORY


data Memory =
  Memory
    { _state :: MVar State
    , _worker :: Chan (IO ())
    }


data State =
  State
    { _history :: History
    , _packages :: Map.Map Pkg.Name Summary
    }



-- SUMMARY


data Summary =
  Summary
    { _versions :: [Pkg.Version]
    , _details :: Maybe ( Text.Text, License.License )
    }


toSummary :: Pkg.Name -> [Pkg.Version] -> IO Summary
toSummary name versions =
  do  let path = toElmJsonPath name (maximum versions)
      bytes <- BS.readFile path
      case Decode.parse Project.pkgDecoder bytes of
        Left _ ->
          return (Summary versions Nothing)

        Right (Project.PkgInfo _ summary license _ _ _ _ constraint) ->
          return $ Summary versions $
            if Con.goodElm constraint then
              Just ( summary, license )
            else
              Nothing


toElmJsonPath :: Pkg.Name -> Pkg.Version -> FilePath
toElmJsonPath name version =
  "packages" </> Pkg.toFilePath name </> Pkg.versionToString version </> "elm.json"



-- INIT


init :: IO Memory
init =
  do  history <- History.load
      packages <- Map.traverseWithKey toSummary (History.groupByName history)

      state <- newMVar (State history packages)
      worker <- newChan
      _ <- forkIO $ forever (join (readChan worker))

      generateAllPackagesJson packages
      Sitemap.generate _versions packages
      generateSearchJson packages

      return $ Memory state worker



-- PUBLIC HELPERS


getHistory :: Memory -> Snap History
getHistory (Memory mvar _) =
  liftIO (_history <$> readMVar mvar)


getPackages :: Memory -> Snap (Map.Map Pkg.Name Summary)
getPackages (Memory mvar _) =
  liftIO (_packages <$> readMVar mvar)


addPackage :: Memory -> Project.PkgInfo -> Snap ()
addPackage (Memory mvar worker) info@(Project.PkgInfo name _ _ version _ _ _ _) =
  liftIO $
  do  (State history packages) <- takeMVar mvar

      let newHistory = History.add name version history
      let newPackages = Map.alter (Just . add info) name packages

      generateAllPackagesJson newPackages
      writeChan worker $
        do  Sitemap.generate _versions newPackages
            generateSearchJson newPackages

      putMVar mvar $ State newHistory newPackages


add :: Project.PkgInfo -> Maybe Summary -> Summary
add (Project.PkgInfo _ summary license version _ _ _ constraint) maybeSummary =
  Summary (maybe [] _versions maybeSummary ++ [version]) $
    if Con.goodElm constraint then
      Just ( summary, license )
    else
      Nothing



-- GENERATE all-packages.json


generateAllPackagesJson :: Map.Map Pkg.Name Summary -> IO ()
generateAllPackagesJson packages =
  write "all-packages.json" $
    Encode.dict Pkg.toString (Encode.list Pkg.encodeVersion . _versions) packages



-- GENERATE search.json


generateSearchJson :: Map.Map Pkg.Name Summary -> IO ()
generateSearchJson packages =
  write "search.json" $
    Encode.list id (Maybe.mapMaybe maybeEncodeSummary (Map.toList packages))


maybeEncodeSummary :: ( Pkg.Name, Summary ) -> Maybe Encode.Value
maybeEncodeSummary ( name, Summary versions maybeDetails ) =
  case maybeDetails of
    Nothing ->
      Nothing

    Just ( summary, license ) ->
      Just $ Encode.object $
        [ ( "name", Pkg.encode name )
        , ( "summary", Encode.text summary )
        , ( "license", License.encode license )
        , ( "versions", Encode.list Pkg.encodeVersion (toVersionHighlights versions) )
        ]


toVersionHighlights :: [Pkg.Version] -> [Pkg.Version]
toVersionHighlights versions =
  reverse $ map last $ take 3 $ reverse $ List.groupBy sameMajor $ List.sort versions


sameMajor :: Pkg.Version -> Pkg.Version -> Bool
sameMajor v1 v2 =
  Pkg._major v1 == Pkg._major v2


write :: FilePath -> Encode.Value -> IO ()
write path json =
  do  let temp = "temp-" ++ path
      Encode.writeUgly temp json
      Dir.renameFile temp path
