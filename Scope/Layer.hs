{-# LANGUAGE RecordWildCards #-}
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
    , plotLayers
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM, (>=>))
import Control.Monad.Trans (lift)
import Data.Function (on)
import qualified Data.IntMap as IM
import qualified Data.Iteratee as I
import Data.List (groupBy)
import Data.Maybe (fromJust)
import Data.Time.Clock
import Data.ZoomCache.Multichannel
import Data.ZoomCache.Numeric
import qualified System.Random.MWC as MWC

import Scope.Plot
import Scope.Types hiding (b)
import Scope.View

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

layersFromFile :: FilePath -> IO ([ScopeLayer], Maybe (TimeStamp, TimeStamp), Maybe (UTCTime, UTCTime))
layersFromFile path = do
    cf <- I.fileDriverRandom (iterHeaders standardIdentifiers) path
    let base   = baseUTC . cfGlobal $ cf
        tracks = IM.keys . cfSpecs $ cf
    colors <- genColors (length tracks) (0.9, 0.9, 0.9) (0.5)
    foldl1 merge <$> mapM (\t -> I.fileDriverRandom (iterListLayers base t) path)
                          (zip tracks colors)
    where
        merge :: ([ScopeLayer], Maybe (TimeStamp, TimeStamp), Maybe (UTCTime, UTCTime))
              -> ([ScopeLayer], Maybe (TimeStamp, TimeStamp), Maybe (UTCTime, UTCTime))
              -> ([ScopeLayer], Maybe (TimeStamp, TimeStamp), Maybe (UTCTime, UTCTime))
        merge (ls1, bs1, ubs1) (ls2, bs2, ubs2) =
            (ls1 ++ ls2, unionBounds bs1 bs2, unionBounds ubs1 ubs2)

        iterListLayers base (trackNo, color) = listLayers base trackNo color <$>
            wholeTrackSummaryListDouble standardIdentifiers trackNo

        listLayers :: Maybe UTCTime -> TrackNo -> RGB -> [Summary Double]
                   -> ([ScopeLayer], Maybe (TimeStamp, TimeStamp), Maybe (UTCTime, UTCTime))
        listLayers base trackNo rgb ss = ([ ScopeLayer (rawListLayer base trackNo ss)
                                          , ScopeLayer (sListLayer base trackNo rgb ss)
                                          ]
                                         , Just (entry, exit)
                                         , utcBounds (entry, exit) <$> base)
            where
                s = head ss
                entry = summaryEntry s
                exit = summaryExit s
                utcBounds (t1, t2) b = (ub t1, ub t2)
                    where
                        ub = utcTimeFromTimeStamp b

        rawListLayer :: Maybe UTCTime -> TrackNo
                     -> [Summary Double] -> Layer (TimeStamp, [Double])
        rawListLayer base trackNo ss = Layer path trackNo
            base
            (summaryEntry s) (summaryExit s)
            enumListDouble (LayerFold (plotRawList (maxRange ss)) plotRawListInit Nothing)
            where
                s = head ss

        sListLayer :: Maybe UTCTime -> TrackNo -> RGB
                   -> [Summary Double] -> Layer [Summary Double]
        sListLayer base trackNo (r, g, b) ss = Layer path trackNo
            base
            (summaryEntry s) (summaryExit s)
            (enumSummaryListDouble 1)
            (LayerFold (plotSummaryList (maxRange ss)) (plotSummaryListInit r g b) Nothing)
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

addLayersFromFile :: FilePath -> Scope ui -> IO (Scope ui)
addLayersFromFile path scope = do
    (newLayers, newBounds, newUTCBounds) <- layersFromFile path
    let oldBounds = bounds scope
        oldUTCBounds = utcBounds scope
        mb = unionBounds oldBounds newBounds
        umb = unionBounds oldUTCBounds newUTCBounds
        t = case oldBounds of
                Just ob -> if oldBounds == mb
                               then id
                               else scopeTransform (mkTSDataTransform ob (fromJust mb))
                _ -> id
    return $ (t scope) { layers = layers scope ++ newLayers
                       , bounds = mb
                       , utcBounds = umb
                       }

----------------------------------------------------------------

plotLayers :: ScopeRender m => Scope ui -> m ()
plotLayers scope = mapM_ f layersByFile
    where
        f :: ScopeRender m => [ScopeLayer] -> m ()
        f ls = plotFileLayers (fn . head $ ls) ls scope
        layersByFile = groupBy ((==) `on` fn) (layers scope)
        fn (ScopeLayer l) = filename l

plotFileLayers :: ScopeRender m => FilePath -> [ScopeLayer] -> Scope ui -> m ()
plotFileLayers path layers scope =
    flip I.fileDriverRandom path $ do
        I.joinI $ enumCacheFile identifiers $ do
            seekTimeStamp (viewStartTime scope (view scope))
            I.joinI . (I.takeWhileE (before (viewEndTime scope v)) >=> I.take 1) $ I.sequence_ is
    where
        v = view scope
        identifiers = standardIdentifiers
        is = map (plotLayer scope) layers

plotLayer :: ScopeRender m => Scope ui -> ScopeLayer -> I.Iteratee [Stream] m ()
plotLayer scope (ScopeLayer Layer{..}) =
    I.joinI . filterTracks [layerTrackNo] . I.joinI . convEnee $ render plotter
    where
        render (LayerMap f initCmds) = do
            d0'm <- I.tryHead
            case d0'm of
                Just d0 -> do
                    asdf <- I.foldM renderMap (toX d0, initCmds)
                    lift $ mapM_ renderCmds (snd asdf)
                Nothing -> return ()
            where
                renderMap (x0, prev) d = do
                    let x = toX d
                        cmds = f x0 (x-x0) d
                    return (x, zipWith (++) prev cmds)
        render (LayerFold f initCmds b00) = do
            d0'm <- I.tryHead
            case d0'm of
                Just d0 -> do
                    asdf <- I.foldM renderFold (toX d0, initCmds, b00)
                    lift $ mapM_ renderCmds (mid asdf)
                Nothing -> return ()
            where
                renderFold (x0, prev, b0) d = do
                    let x = toX d
                        (cmds, b) = f x0 (x-x0) b0 d
                    return (x, zipWith (++) prev cmds, b)
                mid (_,x,_) = x

        toX :: Timestampable a => a -> Double
        toX = toDouble . timeStampToCanvas scope . fromJust . timestamp
