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
      timeStampToData

    -- * Motion, zooming
    , viewAlign
    , viewMoveTo
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

canvasToData :: View -> CanvasX -> DataX
canvasToData View{..} (CanvasX cX) = translate viewX1 $
    DataX (cX * toDouble (distance viewX1 viewX2))

timeStampToData :: Scope -> TimeStamp -> Maybe DataX
timeStampToData Scope{..} (TS ts) = fmap tsToData bounds
    where
        tsToData :: (TimeStamp, TimeStamp) -> DataX
        tsToData (TS t1, TS t2) = DataX (ts - t1 / t2 - t1)

viewSetEnds :: DataX -> DataX -> View -> View
viewSetEnds x1 x2 v@View{..} = v { viewX1 = x1, viewX2 = x2 }

-- | Align a view so the given DataX appears at CanvasX,
-- preserving the current view width.
viewAlign :: CanvasX -> DataX -> View -> View
viewAlign (CanvasX cx) (DataX dx) v@View{..} = viewSetEnds (DataX newX1') (DataX newX2') v
    where
        DataX vW = distance viewX1 viewX2 -- current width of view window
        newX1 = max 0 $ dx - (cx * vW)
        newX2 = newX1 + vW
        (newX1', newX2') = restrictPair01 (newX1, newX2)

viewMoveTo :: Double -> View -> View
viewMoveTo val v@View{..} = viewSetEnds newX1' newX2' v
    where
        (newX1', newX2') = restrictPair01 .
            translatePair (distance viewX1 (DataX val)) $
            (viewX1, viewX2)

viewZoomOutOn :: CanvasX -> Double -> View -> View
viewZoomOutOn focus mult v@View{..} = viewSetEnds newX1 newX2' v
    where
        (newX1, newX2') = restrictPair01 $
            zoomPair focus mult (viewX1, viewX2)

viewButtonDown :: CanvasX -> View -> View
viewButtonDown cX v = v { dragDX = Just (canvasToData v cX) }

viewButtonMotion :: CanvasX -> View -> View
viewButtonMotion cX v@View{..} = viewAlign cX (fromJust dragDX) v

viewButtonRelease :: View -> View
viewButtonRelease v = v { dragDX = Nothing}

----------------------------------------------------------------------
