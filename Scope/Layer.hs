{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
{- |
   Module      : Scope.Layer
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

   Layers

-}
----------------------------------------------------------------------

module Scope.Layer (
    -- * Layers
      addLayersFromFile
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import qualified Data.IntMap as IM
import qualified Data.Iteratee as I
import Data.Maybe (fromJust)
import Data.ZoomCache.Multichannel
import Data.ZoomCache.Numeric
import qualified System.Random.MWC as MWC

import Scope.Plot
import Scope.Types hiding (b)

----------------------------------------------------------------------
-- Random, similar colors

type RGB = (Double, Double, Double)

genColor :: RGB -> Double -> MWC.GenIO -> IO RGB
genColor (r, g, b) a gen = do
    let a' = 1.0 - a
    r' <- MWC.uniformR (0.0, a') gen
    g' <- MWC.uniformR (0.0, a') gen
    b' <- MWC.uniformR (0.0, a') gen
    return (r*a + r', g*a + g', b*a * b')

genColors :: Int -> RGB -> Double -> IO [RGB]
genColors n rgb a = MWC.withSystemRandom (replicateM n . genColor rgb a)

----------------------------------------------------------------------

layersFromFile :: FilePath -> IO ([ScopeLayer], Maybe (TimeStamp, TimeStamp))
layersFromFile path = do
    tracks <- IM.keys . cfSpecs <$> I.fileDriverRandom (iterHeaders standardIdentifiers) path
    colors <- genColors (length tracks) (0.9, 0.9, 0.9) (0.5)
    -- foldl1 merge <$> mapM (\t -> I.fileDriverRandom (iterLayers t) path) (zip tracks colors)
    foldl1 merge <$> mapM (\t -> I.fileDriverRandom (iterListLayers t) path) (zip tracks colors)
    where
        merge :: ([ScopeLayer], Maybe (TimeStamp, TimeStamp))
              -> ([ScopeLayer], Maybe (TimeStamp, TimeStamp))
              -> ([ScopeLayer], Maybe (TimeStamp, TimeStamp))
        merge (ls1, bs1) (ls2, bs2) = (ls1 ++ ls2, unionBounds bs1 bs2)

{-
        iterLayers (trackNo, color) = layers trackNo color <$>
            wholeTrackSummaryListDouble standardIdentifiers trackNo

        layers :: TrackNo -> RGB -> Summary Double -> ([ScopeLayer], Maybe (TimeStamp, TimeStamp))
        layers trackNo rgb s = ([ ScopeLayer (rawLayer trackNo s)
                                , ScopeLayer (sLayer trackNo rgb s)
                                ]
                               , Just (summaryEntry s, summaryExit s))

        rawLayer :: TrackNo -> Summary Double -> Layer (TimeStamp, Double)
        rawLayer trackNo s = Layer path trackNo (summaryEntry s) (summaryExit s)
            enumDouble (LayerFold (plotRaw (yRange s)) Nothing)

        sLayer :: TrackNo -> RGB -> Summary Double -> Layer (Summary Double)
        sLayer trackNo (r, g, b) s = Layer path trackNo (summaryEntry s) (summaryExit s)
            (enumSummaryDouble 1) (LayerFold (plotSummary (yRange s) r g b) Nothing)
-}

        iterListLayers (trackNo, color) = listLayers trackNo color <$>
            wholeTrackSummaryListDouble standardIdentifiers trackNo

        listLayers :: TrackNo -> RGB -> [Summary Double] -> ([ScopeLayer], Maybe (TimeStamp, TimeStamp))
        listLayers trackNo rgb ss = ([ ScopeLayer (rawListLayer trackNo ss)
                                     , ScopeLayer (sListLayer trackNo rgb ss)
                                     ]
                                    , Just (summaryEntry s, summaryExit s))
            where
                s = head ss

        rawListLayer :: TrackNo -> [Summary Double] -> Layer (TimeStamp, [Double])
        rawListLayer trackNo ss = Layer path trackNo (summaryEntry s) (summaryExit s)
            enumListDouble (LayerFold (plotRawList (maxRange ss)) Nothing)
            where
                s = head ss

        sListLayer :: TrackNo -> RGB -> [Summary Double] -> Layer [Summary Double]
        sListLayer trackNo (r, g, b) ss = Layer path trackNo (summaryEntry s) (summaryExit s)
            (enumSummaryListDouble 1) (LayerFold (plotSummaryList (maxRange ss) r g b) Nothing)
            where
                s = head ss

        maxRange :: [Summary Double] -> Double
        maxRange = maximum . map yRange

        yRange :: Summary Double -> Double
        yRange s = 2 * ((abs . numMin . summaryData $ s) + (abs . numMax . summaryData $ s))

unionBounds :: Ord a => Maybe (a, a) -> Maybe (a, a) -> Maybe (a, a)
unionBounds a         Nothing   = a
unionBounds Nothing   b         = b
unionBounds (Just r1) (Just r2) = Just (unionRange r1 r2)

addLayersFromFile :: FilePath -> Scope -> IO Scope
addLayersFromFile path scope = do
    (newLayers, newBounds) <- layersFromFile path
    let oldBounds = bounds scope
        mb = unionBounds oldBounds newBounds
        t = case oldBounds of
                Just ob -> if oldBounds == mb
                               then id
                               else scopeTransform (mkTSDataTransform ob (fromJust mb))
                _ -> id
    return $ (t scope) { layers = layers scope ++ newLayers
                       , bounds = mb
                       }
