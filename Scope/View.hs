{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Scope.View
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Functions for dealing with Views
----------------------------------------------------------------------

module Scope.View (
    -- * Coordinate conversions
      timeStampToData
    , dataToTimeStamp
    , timeStampToCanvas

    , viewStartTime
    , viewEndTime
    , viewDuration

    -- * Motion
    , viewAlign
    , viewMoveStart
    , viewMoveEnd
    , viewMoveLeft
    , viewMoveRight
    , viewMoveTo

    -- * Zoom
    , viewZoomIn
    , viewZoomOut
    , viewZoomInOn
    , viewZoomOutOn

    -- * Button handling
    , viewButtonDown
    , viewButtonMotion
    , viewButtonRelease
) where

import Data.Maybe (fromJust)
import Data.ZoomCache

import Scope.Types

----------------------------------------------------------------------

canvasToData :: View ui -> CanvasX -> DataX
canvasToData View{..} (CanvasX cX) = translate viewX1 $
    DataX (cX * toDouble (distance viewX1 viewX2))

timeStampToData :: Scope ui -> TimeStamp -> Maybe DataX
timeStampToData Scope{..} (TS ts) = fmap tsToData bounds
    where
        tsToData :: (TimeStamp, TimeStamp) -> DataX
        tsToData (TS t1, TS t2) = DataX $ ts - t1 / (t2 - t1)

dataToTimeStamp :: Scope ui -> DataX -> Maybe TimeStamp
dataToTimeStamp Scope{..} (DataX dX) = fmap dataToTS bounds
    where
        dataToTS :: (TimeStamp, TimeStamp) -> TimeStamp
        dataToTS (TS t1, TS t2) = TS $ t1 + dX * (t2 - t1)

timeStampToCanvas :: Scope ui -> TimeStamp -> CanvasX
timeStampToCanvas scope ts = CanvasX $
    toDouble (distance vt1 ts) / toDouble (distance vt1 vt2)
    where
        v = view scope
        vt1 = fromJust $ dataToTimeStamp scope (viewX1 v)
        vt2 = fromJust $ dataToTimeStamp scope (viewX2 v)

----------------------------------------------------------------------

viewStartTime :: Scope ui -> View ui -> Maybe TimeStamp
viewStartTime scope View{..} = dataToTimeStamp scope viewX1

viewEndTime :: Scope ui -> View ui -> Maybe TimeStamp
viewEndTime scope View{..} = dataToTimeStamp scope viewX2

viewDuration :: Scope ui -> View ui -> Maybe TimeStampDiff
viewDuration scope view =
    case (viewStartTime scope view, viewEndTime scope view) of
        (Just s, Just e) -> Just $ timeStampDiff e s
        _                -> Nothing

----------------------------------------------------------------------

viewSetEnds :: DataX -> DataX -> View ui -> View ui
viewSetEnds x1 x2 v@View{..} = v { viewX1 = x1, viewX2 = x2 }

-- | Align a view so the given DataX appears at CanvasX,
-- preserving the current view width.
viewAlign :: CanvasX -> DataX -> View ui -> View ui
viewAlign (CanvasX cx) (DataX dx) v@View{..} = viewSetEnds (DataX newX1') (DataX newX2') v
    where
        DataX vW = distance viewX1 viewX2 -- current width of view window
        newX1 = max 0 $ dx - (cx * vW)
        newX2 = newX1 + vW
        (newX1', newX2') = restrictRange01 (newX1, newX2)

viewMoveStart :: View ui -> View ui
viewMoveStart = viewAlign (CanvasX 0.0) (DataX 0.0)

viewMoveEnd :: View ui -> View ui
viewMoveEnd = viewAlign (CanvasX 1.0) (DataX 1.0)

viewMoveLeft :: View ui -> View ui
viewMoveLeft v@View{..} = viewAlign (CanvasX 0.0) viewX2 v

viewMoveRight :: View ui -> View ui
viewMoveRight v@View{..} = viewAlign (CanvasX 1.0) viewX1 v

viewMoveTo :: Double -> View ui -> View ui
viewMoveTo val v@View{..} = viewSetEnds newX1' newX2' v
    where
        (newX1', newX2') = restrictRange01 .
            translateRange (distance viewX1 (DataX val)) $
            (viewX1, viewX2)

viewZoomIn :: Double -> View ui -> View ui
viewZoomIn = viewZoomInOn (CanvasX 0.5)

viewZoomInOn :: CanvasX -> Double -> View ui -> View ui
viewZoomInOn focus mult = viewZoomOutOn focus (1.0/mult)

viewZoomOut :: Double -> View ui -> View ui
viewZoomOut = viewZoomOutOn (CanvasX 0.5)

viewZoomOutOn :: CanvasX -> Double -> View ui -> View ui
viewZoomOutOn focus mult v@View{..} = viewSetEnds newX1 newX2' v
    where
        (newX1, newX2') = restrictRange01 $
            zoomRange focus mult (viewX1, viewX2)

viewButtonDown :: CanvasX -> View ui -> View ui
viewButtonDown cX v = v { dragDX = Just (canvasToData v cX) }

viewButtonMotion :: CanvasX -> View ui -> View ui
viewButtonMotion cX v@View{..} = case dragDX of
    Just dX -> viewAlign cX dX v'
    Nothing -> v'
    where
        v' = v { pointerX = Just cX }

viewButtonRelease :: View ui -> View ui
viewButtonRelease v = v { dragDX = Nothing}

----------------------------------------------------------------------
