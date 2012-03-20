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
import Control.Monad (foldM, join, replicateM, (>=>))
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
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
import System.Posix
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

scopeBufSize :: Int
scopeBufSize = 1024

openScopeFile :: FilePath -> IO ScopeFile
openScopeFile path = do
    fd <- openFd path ReadOnly Nothing defaultFileFlags
    let f = ScopeFile path fd undefined
    cf <- scopeEnum f (iterHeaders standardIdentifiers)
    return f{scopeCF = cf}

scopeEnum :: ScopeRender m => ScopeFile -> I.Iteratee (Offset ByteString) m a -> m a
scopeEnum ScopeFile{..} iter = OffI.enumFdRandomOBS scopeBufSize fd iter >>= I.run

layersFromFile :: ScopeFile -> IO ([ScopeLayer], Maybe (TimeStamp, TimeStamp), Maybe (UTCTime, UTCTime))
layersFromFile file@ScopeFile{..} = do
    let base   = baseUTC . cfGlobal $ scopeCF
        tracks = IM.keys . cfSpecs $ scopeCF
    colors <- genColors (length tracks) (0.9, 0.9, 0.9) (0.5)
    foldl1 merge <$> mapM (\t -> scopeEnum file (I.joinI $ enumBlock scopeCF $ iterListLayers base t))
                          (zip tracks colors)
    where
        merge :: ([ScopeLayer], Maybe (TimeStamp, TimeStamp), Maybe (UTCTime, UTCTime))
              -> ([ScopeLayer], Maybe (TimeStamp, TimeStamp), Maybe (UTCTime, UTCTime))
              -> ([ScopeLayer], Maybe (TimeStamp, TimeStamp), Maybe (UTCTime, UTCTime))
        merge (ls1, bs1, ubs1) (ls2, bs2, ubs2) =
            (ls1 ++ ls2, unionBounds bs1 bs2, unionBounds ubs1 ubs2)

        iterListLayers base (trackNo, color) = listLayers base trackNo color <$>
            wholeTrackSummaryListDouble trackNo

        listLayers :: Maybe UTCTime -> TrackNo -> RGB -> [Summary Double]
                   -> ([ScopeLayer], Maybe (TimeStamp, TimeStamp), Maybe (UTCTime, UTCTime))
        listLayers base trackNo rgb ss = ([ ScopeLayer (rawListLayer base trackNo extents)
                                          , ScopeLayer (sListLayer base trackNo rgb extents)
                                          ]
                                         , Just (entry, exit)
                                         , utcBounds (entry, exit) <$> base)
            where
                extents = LayerExtents entry exit (maxRange ss)
                s = head ss
                entry = summaryEntry s
                exit = summaryExit s
                utcBounds (t1, t2) b = (ub t1, ub t2)
                    where
                        ub = utcTimeFromTimeStamp b

        rawListLayer :: Maybe UTCTime -> TrackNo
                     -> LayerExtents -> Layer (TimeStamp, [Double])
        rawListLayer base trackNo extents = Layer file trackNo
            base
            extents
            enumListDouble
            (rawLayerPlot extents (0,0,0))

        sListLayer :: Maybe UTCTime -> TrackNo -> RGB
                   -> LayerExtents -> Layer [Summary Double]
        sListLayer base trackNo rgb extents = Layer file trackNo
            base
            extents
            (enumSummaryListDouble 1)
            (summaryLayerPlot extents rgb)

        maxRange :: [Summary Double] -> Double
        maxRange = maximum . map yRange

        yRange :: Summary Double -> Double
        yRange s = 2 * ((abs . numMin . summaryData $ s) + (abs . numMax . summaryData $ s))

addLayersFromFile :: FilePath -> Scope ui -> IO (Scope ui)
addLayersFromFile path scope = do
    (newLayers, newBounds, newUTCBounds) <- layersFromFile =<< openScopeFile path
    let scope' = scopeUpdate newBounds newUTCBounds scope
    return $ scope' { layers = layers scope ++ newLayers }

----------------------------------------------------------------

plotLayers :: ScopeRender m => Scope ui -> m (Scope ui)
plotLayers scope0 = foldM f scope0 layersByFile
    where
        f :: ScopeRender m => Scope ui -> [ScopeLayer] -> m (Scope ui)
        f scope ls = do
            file' <- plotFileLayers (lf . head $ ls) ls scope
            return (updateFiles file' scope)

        updateFiles :: ScopeFile -> Scope ui -> Scope ui
        updateFiles file scope = scope { layers = map u (layers scope) }
            where
                u (ScopeLayer l)
                    | (fd . layerFile $ l) == (fd file)
                        = ScopeLayer l{layerFile = file}
                    | otherwise
                        = ScopeLayer l

        layersByFile :: [[ScopeLayer]]
        layersByFile = groupBy ((==) `on` (fd . lf)) (layers scope0)
        lf (ScopeLayer l) = layerFile l

plotFileLayers :: ScopeRender m => ScopeFile -> [ScopeLayer] -> Scope ui -> m ScopeFile
plotFileLayers file layers scope =
    if (any visible layers)
        then scopeEnum file $ do
            I.seek 0
            I.joinI $ enumBlock (scopeCF file) $ do
                seekTimeStamp (scopeCF file) seekStart
                I.joinI . (I.takeWhileE (before seekEnd) >=> I.take 1) $ I.sequence_ is
                cf <- maybe (scopeCF file) (blkFile . unwrapOffset) <$> I.peek
                return file{scopeCF = cf}
        else return file
    where
        v = view scope
        is = map (plotLayer scope) layers

        visible (ScopeLayer Layer{..}) =
            maybe False (< (endTime layerExtents)) seekStart &&
            maybe False (> (startTime layerExtents)) seekEnd

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
