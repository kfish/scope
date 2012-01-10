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

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (join, replicateM, (>=>))
import Control.Monad.Trans (lift)
import Data.Function (on)
import qualified Data.IntMap as IM
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO.OffsetFd as OffI
import Data.List (groupBy)
import Data.Maybe (fromJust, listToMaybe)
import Data.Offset
import Data.Time.Clock
import Data.ZoomCache.Multichannel
import Data.ZoomCache.Numeric
import qualified System.Random.MWC as MWC

import Scope.Numeric.IEEE754()
import Scope.Types hiding (b)
import Scope.View

----------------------------------------------------------------------
-- Random, similar colors

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
    cf <- OffI.fileDriverRandomOBS (iterHeaders standardIdentifiers) path
    let base   = baseUTC . cfGlobal $ cf
        tracks = IM.keys . cfSpecs $ cf
    colors <- genColors (length tracks) (0.9, 0.9, 0.9) (0.5)
    foldl1 merge <$> mapM (\t -> OffI.fileDriverRandomOBS (iterListLayers base t) path)
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
            enumListDouble
            (rawLayerPlot (maxRange ss) (0,0,0))
            where
                s = head ss

        sListLayer :: Maybe UTCTime -> TrackNo -> RGB
                   -> [Summary Double] -> Layer [Summary Double]
        sListLayer base trackNo rgb ss = Layer path trackNo
            base
            (summaryEntry s) (summaryExit s)
            (enumSummaryListDouble 1)
            (summaryLayerPlot (maxRange ss) rgb)
            where
                s = head ss

        maxRange :: [Summary Double] -> Double
        maxRange = maximum . map yRange

        yRange :: Summary Double -> Double
        yRange s = 2 * ((abs . numMin . summaryData $ s) + (abs . numMax . summaryData $ s))

addLayersFromFile :: FilePath -> Scope ui -> IO (Scope ui)
addLayersFromFile path scope = do
    (newLayers, newBounds, newUTCBounds) <- layersFromFile path
    let scope' = scopeUpdate newBounds newUTCBounds scope
    return $ scope' { layers = layers scope ++ newLayers }

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
    flip OffI.fileDriverRandomOBS path $ do
        I.joinI $ enumCacheFile identifiers $ do
            seekTimeStamp seekStart
            I.joinI . (I.takeWhileE (before seekEnd) >=> I.take 1) $ I.sequence_ is
    where
        v = view scope
        identifiers = standardIdentifiers
        is = map (plotLayer scope) layers

        seekStart = ts (viewStartUTC scope v) <|> viewStartTime scope v
        seekEnd = ts (viewEndUTC scope v) <|> viewEndTime scope v

        ts = (timeStampFromUTCTime <$> base <*>)
        base :: Maybe UTCTime
        base = join . listToMaybe $ lBase <$> take 1 layers
        lBase (ScopeLayer l) = layerBaseUTC l

plotLayer :: ScopeRender m => Scope ui -> ScopeLayer -> I.Iteratee [Offset Block] m ()
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
        toX = case (utcBounds scope, layerBaseUTC) of
                  (Just _, Just base) -> toUTCX base
                  _                   -> toTSX

        toTSX :: Timestampable a => a -> Double
        toTSX = toDouble . timeStampToCanvas scope . fromJust . timestamp

        toUTCX :: Timestampable a => UTCTime -> a -> Double
        toUTCX base = toDouble . utcToCanvas scope . utcTimeFromTimeStamp base . fromJust . timestamp
