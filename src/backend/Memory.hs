{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.ByteString as BS
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
    , _weight :: Int
    }


toSummary :: Pkg.Name -> [Pkg.Version] -> IO Summary
toSummary name versions =
  do  let path = toElmJsonPath name (maximum versions)
      bytes <- BS.readFile path
      case Decode.parse "summary" (const []) Project.pkgDecoder bytes of
        Left _ ->
          return (Summary versions Nothing (-1))

        Right (Project.PkgInfo _ summary license _ _ _ _ constraint) ->
          let
            details =
              if Con.goodElm constraint
                then Just ( summary, license )
                else Nothing
          in
          return $ Summary versions details (getWeight name)


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
      generateSitemap packages
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
        do  generateSitemap newPackages
            generateSearchJson newPackages

      putMVar mvar $ State newHistory newPackages


add :: Project.PkgInfo -> Maybe Summary -> Summary
add (Project.PkgInfo name summary license version _ _ _ constraint) maybeSummary =
  let
    versions =
      maybe [] _versions maybeSummary ++ [version]

    details =
      if Con.goodElm constraint then
        Just ( summary, license )
      else
        Nothing
  in
  Summary versions details (getWeight name)




-- GENERATE all-packages.json


generateAllPackagesJson :: Map.Map Pkg.Name Summary -> IO ()
generateAllPackagesJson packages =
  write "all-packages.json" $
    Encode.dict Pkg.toText (Encode.list Pkg.encodeVersion . _versions) packages




-- GENERATE sitemap.xml


generateSitemap :: Map.Map Pkg.Name Summary -> IO ()
generateSitemap packages =
  Sitemap.generate (Map.filter (Maybe.isJust . _details) packages)



-- GENERATE search.json


generateSearchJson :: Map.Map Pkg.Name Summary -> IO ()
generateSearchJson packages =
  write "search.json" $
    Encode.list id $ Maybe.mapMaybe maybeEncodeSummary $
      List.sortOn (negate . _weight . snd) (Map.toList packages)


maybeEncodeSummary :: ( Pkg.Name, Summary ) -> Maybe Encode.Value
maybeEncodeSummary ( name, Summary versions maybeDetails _ ) =
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



-- WEIGHTS


getWeight :: Pkg.Name -> Int
getWeight (Pkg.Name author _) =
  Map.findWithDefault 0 author weights


-- Currently based on appearances at elm-conf and Elm Europe.
--
-- The theory is that this selects packages by authors that have a real
-- relationship with the Elm community. I will list a couple other metrics
-- that I think are weaker:
--
-- GitHub stars - This basically measures if your README ever got posted on
--   reddit or Hacker News. This would prioritize "able to get on HN" over
--   questions like "how nice is the API?" and "how well does it fit with the
--   Elm ecosystem?"
--
-- Download counts - This basically measures CI builds. Furthermore, Elm 0.19
--   will only download a package once per user. That means everyone downloads
--   elm/core 6.0.0 once ever. If your CI is configured correctly, it
--   should be caching to avoid downloads and builds. So in the end this would
--   measure "have people tried it out once or more?" with a heavy bias towards
--   misconfigured CI builds.
--
-- Time spent in docs - This measures how much time people spend learning a
--   package. So folks probably are not in elm/core very often after a
--   certain point, but it is very important! Folks may be in docs because they
--   do not use it often and forgot or find it really confusing.
--
-- By going with conference talks, I think we capture important details with
-- higher fidelity. Is this person invested in the Elm ecosystem? Will they be
-- likely to be hooked into core development and update packages promptly? Have
-- they thought about their design carefully enough to make a talk of it? Etc.
--
weights :: Map.Map Text.Text Int
weights =
  Map.insert "elm" 100000 $
  Map.insert "elm-explorations" 1000 $
    foldr (\author dict -> Map.insertWith (+) author 1 dict) Map.empty $
      -- elm-conf 2016 - https://2016.elm-conf.us/speaker/
      [ "evancz"
      , "ohanhi"
      , "lukewestby"
      , "tesk9"
      , "mdgriffith"
      , "abadi199"
      , "JoelQ"
      , "jschomay"
      , "jessitron"
      , "splodingsocks"
      , "rtfeldman"
      -- elm-conf 2017 - https://2017.elm-conf.us/talks/
      , "evancz"
      , "klaftertief"
      , "tesk9"
      , "splodingsocks"
      , "lukewestby"
      , "jfairbank"
      , "pzingg"
      , "w0rm"
      , "terezka"
      , "rtfeldman"
      -- Elm Europe 2017 - https://2017.elmeurope.org/
      , "evancz"
      , "rtfeldman"
      , "Janiczek"
      , "eeue56"
      , "Skinney"
      , "pickled"
      , "thebritican"
      , "gampleman"
      , "amitaibu"
      , "jsteiner"
      , "jschomay"
      , "supermario"
      , "mdgriffith"
      , "BrianHicks"
      , "Fenntasy"
      , "tomekwi"
      , "terezka"
      , "myrho"
      , "w0rm"
      , "noahzgordon"
      , "sebcreme"
      , "cbenz"
      -- Elm Europe 2018 - https://2018.elmeurope.org/
      , "evancz"
      , "rtfeldman"
      , "Janiczek"
      , "jxxcarlson"
      , "BrianHicks"
      , "bakkdoor"
      , "JoelQ"
      , "ianmackenzie"
      , "lukewestby"
      , "mdgriffith"
      , "paulsonnentag"
      , "Arkham"
      , "bitterjug"
      , "decioferreira"
      , "emmacunningham"
      , "tibastral"
      , "kachkaev"
      , "supermario"
      , "myrho"
      , "w0rm"
      , "celine-m-s"
      ]
