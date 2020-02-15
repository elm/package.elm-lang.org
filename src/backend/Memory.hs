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
import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Utf8 as Utf8
import Snap.Core (Snap)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO.Streams.ByteString as SB
import qualified System.IO.Streams.Core as S
import qualified System.IO.Streams.File as SF
import qualified System.IO.Streams.Zlib as SZ

import qualified Elm.Constraint as C
import qualified Elm.Licenses as License
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Json.Decode as D
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Json.String as Json

import qualified Gzip
import qualified Helpers
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
    { _versions :: [V.Version]
    , _details :: Maybe ( Json.String, License.License )
    , _weight :: Int
    }


toSummary :: Pkg.Name -> [V.Version] -> IO Summary
toSummary pkg versions =
  do  bytes <- Gzip.inflateFile $ "packages" </> Pkg.toFilePath pkg </> V.toChars (maximum versions) </> "elm.json.gz"
      case D.fromByteString Outline.decoder bytes of
        Left _ ->
          return (Summary versions Nothing (-1))

        Right (Outline.App _) ->
          return (Summary versions Nothing (-1))

        Right (Outline.Pkg (Outline.PkgOutline _ summary license _ _ _ _ elmConstraint)) ->
          let
            details =
              if is19 elmConstraint
                then Just ( summary, license )
                else Nothing
          in
          return $ Summary versions details (getWeight pkg)



-- INIT


init :: IO Memory
init =
  Helpers.report "Memory" $
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


addPackage :: Memory -> Outline.PkgOutline -> Snap ()
addPackage (Memory mvar worker) outline@(Outline.PkgOutline pkg _ _ vsn _ _ _ _) =
  liftIO $
  do  (State history packages) <- takeMVar mvar

      let newHistory = History.add pkg vsn history
      let newPackages = Map.alter (Just . add outline) pkg packages

      generateAllPackagesJson newPackages
      writeChan worker $
        do  generateSitemap newPackages
            generateSearchJson newPackages

      putMVar mvar $ State newHistory newPackages


add :: Outline.PkgOutline -> Maybe Summary -> Summary
add (Outline.PkgOutline pkg summary license vsn _ _ _ elmConstraint) maybeSummary =
  let
    versions =
      maybe [] _versions maybeSummary ++ [vsn]

    details =
      if is19 elmConstraint then
        Just ( summary, license )
      else
        Nothing
  in
  Summary versions details (getWeight pkg)


is19 :: C.Constraint -> Bool
is19 elmConstraint =
  any (C.satisfies elmConstraint) [ V.Version 0 19 0, V.Version 0 19 1 ]



-- GENERATE all-packages.json


generateAllPackagesJson :: Map.Map Pkg.Name Summary -> IO ()
generateAllPackagesJson packages =
  write "all-packages.json" $
    E.dict Pkg.toJsonString (E.list V.encode . _versions) packages


write :: FilePath -> E.Value -> IO ()
write path json =
  do  let temp = "temp-" ++ path
      E.writeUgly temp json
      Dir.renameFile temp path



-- GENERATE sitemap.xml


generateSitemap :: Map.Map Pkg.Name Summary -> IO ()
generateSitemap packages =
  Sitemap.generate (Map.filter (Maybe.isJust . _details) packages)



-- GENERATE search.json


generateSearchJson :: Map.Map Pkg.Name Summary -> IO ()
generateSearchJson packages =
  gzipAndWrite "search.json.gz" $
    E.list id $ Maybe.mapMaybe maybeEncodeSummary $
      List.sortOn (negate . _weight . snd) (Map.toList packages)


maybeEncodeSummary :: ( Pkg.Name, Summary ) -> Maybe E.Value
maybeEncodeSummary ( pkg, Summary versions maybeDetails _ ) =
  case maybeDetails of
    Nothing ->
      Nothing

    Just ( summary, license ) ->
      Just $ E.object $
        [ "name" ==> Pkg.encode pkg
        , "summary" ==> E.string summary
        , "license" ==> License.encode license
        , "version" ==> V.encode (List.maximum versions)
        ]


gzipAndWrite :: FilePath -> E.Value -> IO ()
gzipAndWrite path json =
  do  let temp = "temp-" ++ path
      let lbs = B.toLazyByteString (E.encodeUgly json)
      input <- SB.fromLazyByteString lbs

      SF.withFileAsOutput temp $ \output ->
        S.connect input =<< SZ.gzip (SZ.CompressionLevel 9) output

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
weights :: Map.Map Pkg.Author Int
weights =
  let
    one author =
      ( Utf8.fromChars author, 1 )
  in
  Map.fromListWith (+)
    [ ( Utf8.fromChars "elm", 100000 )
    , ( Utf8.fromChars "elm-explorations", 1000 )
    -- elm-conf 2016 - https://2016.elm-conf.us/speaker/
    , one "evancz"
    , one "ohanhi"
    , one "lukewestby"
    , one "tesk9"
    , one "mdgriffith"
    , one "abadi199"
    , one "JoelQ"
    , one "jschomay"
    , one "jessitron"
    , one "splodingsocks"
    , one "rtfeldman"
    -- elm-conf 2017 - https://2017.elm-conf.us/talks/
    , one "evancz"
    , one "klaftertief"
    , one "tesk9"
    , one "splodingsocks"
    , one "lukewestby"
    , one "jfairbank"
    , one "pzingg"
    , one "w0rm"
    , one "terezka"
    , one "rtfeldman"
    -- Elm Europe 2017 - https://2017.elmeurope.org/
    , one "evancz"
    , one "rtfeldman"
    , one "Janiczek"
    , one "eeue56"
    , one "Skinney"
    , one "pickled"
    , one "thebritican"
    , one "gampleman"
    , one "amitaibu"
    , one "jsteiner"
    , one "jschomay"
    , one "supermario"
    , one "mdgriffith"
    , one "BrianHicks"
    , one "Fenntasy"
    , one "tomekwi"
    , one "terezka"
    , one "myrho"
    , one "w0rm"
    , one "noahzgordon"
    , one "sebcreme"
    , one "cbenz"
    -- Elm Europe 2018 - https://2018.elmeurope.org/
    , one "evancz"
    , one "rtfeldman"
    , one "Janiczek"
    , one "jxxcarlson"
    , one "BrianHicks"
    , one "bakkdoor"
    , one "JoelQ"
    , one "ianmackenzie"
    , one "lukewestby"
    , one "mdgriffith"
    , one "paulsonnentag"
    , one "Arkham"
    , one "bitterjug"
    , one "decioferreira"
    , one "emmacunningham"
    , one "tibastral"
    , one "kachkaev"
    , one "supermario"
    , one "myrho"
    , one "w0rm"
    , one "celine-m-s"
    ]
