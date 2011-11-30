{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Scope.Types
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Scope types and interfaces
----------------------------------------------------------------------

module Scope.Types (
    -- * Coordinates
      Coordinate(..)
    , ScreenX(..)
    , ScreenY(..)
    , CanvasX(..)
    , CanvasY(..)
    , DataX(..)
    , DataY(..)

    , restrictPair
    , restrictPair01
    , translatePair
    , zoomPair

    -- * Scope
    , Scope(..)
    , scopeNew

    -- * Views
    , View(..)

    -- * Layers
    , Layer(..)
    , LayerPlot(..)
    , LayerMapFunc
    , LayerFoldFunc
    , ScopeLayer(..)
) where

import Data.Maybe
import Data.Iteratee (Enumeratee)
import Data.ZoomCache

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk as G

----------------------------------------------------------------------

class Coordinate a where
    fromDouble :: Double -> a
    toDouble :: a -> Double

    -- | Distance from to
    distance :: a -> a -> a
    -- | Translate x by
    translate :: a -> a -> a

{- |

@
           CanvasX 0.0                       CanvasX 1.0    DataX 1.0
              |                                 |              |
 DataX 0.0    V                                 V              V
    |
    V          ---------------------------------   <- CanvasY -1.0
              |                                 |
    +---------+---------------------------------+--------------+
    |         |                                 |              |
    |         |                                 |              |
    +---------+---------------------------------+--------------+
              |                                 |
               ---------------------------------   <- CanvasY -1.0
@

-}

newtype ScreenX = ScreenX Double deriving (Eq, Ord, Show)
newtype ScreenY = ScreenY Double deriving (Eq, Ord, Show)
newtype CanvasX = CanvasX Double deriving (Eq, Ord, Show)
newtype CanvasY = CanvasY Double deriving (Eq, Ord, Show)
newtype DataX   = DataX   Double deriving (Eq, Ord, Show)
newtype DataY   = DataY   Double deriving (Eq, Ord, Show)

instance Coordinate Double where
    fromDouble = id
    toDouble = id
    distance x1 x2 = x2 - x1
    translate t x = x + t

instance Coordinate ScreenX where
    fromDouble d = ScreenX d
    toDouble (ScreenX d) = d
    distance (ScreenX x1) (ScreenX x2) = ScreenX (distance x1 x2)
    translate (ScreenX t) (ScreenX x)  = ScreenX (translate t x)

instance Coordinate CanvasX where
    fromDouble d = CanvasX d
    toDouble (CanvasX d) = d
    distance (CanvasX x1) (CanvasX x2) = CanvasX (distance x1 x2)
    translate (CanvasX t) (CanvasX x)  = CanvasX (translate t x)

instance Coordinate DataX where
    fromDouble d = DataX d
    toDouble (DataX d) = d
    distance (DataX x1) (DataX x2) = DataX (distance x1 x2)
    translate (DataX t) (DataX x)  = DataX (translate t x)

translatePair :: Coordinate a => a -> (a, a) -> (a, a)
translatePair t (x1, x2) = (translate t x1, translate t x2)

-- | Restrict a window to within a given range
restrictPair :: (Ord a, Coordinate a) => (a, a) -> (a, a) -> (a, a)
restrictPair (rangeX1, rangeX2) (x1, x2)
    | w >= rW      = (rangeX1, rangeX2)
    | x1 < rangeX1 = (rangeX1, translate rangeX1 w)
    | x2 > rangeX2 = (x1', rangeX2)
    | otherwise    = (x1, x2)
    where
        rW = distance rangeX1 rangeX2
        w = distance x1 x2
        x1' = distance w rangeX2

restrictPair01 :: (Ord a, Coordinate a) => (a, a) -> (a, a)
restrictPair01 = restrictPair (fromDouble 0.0, fromDouble 1.0)

zoomPair :: Coordinate a => CanvasX -> Double -> (a, a) -> (a, a)
zoomPair (CanvasX focus) mult (x1, x2) = (translate off1 x1, translate off2 x2)
    where
        off1 = fromDouble $ (oldW - newW) * focus
        off2 = fromDouble $ (newW - oldW) * (1.0 - focus)
        oldW = toDouble $ distance x1 x2
        newW = min 1.0 (oldW * mult)

----------------------------------------------------------------------

-- | A layer plotting function which is just given the x position and x width
-- to render the data value of type 'a' into.
type LayerMapFunc a = Double -> Double -> a -> C.Render ()

-- | A layer plotting function which is given the x position and x width,
-- and a previously returned value of type 'b'
type LayerFoldFunc a b = Double -> Double -> b -> a -> C.Render b

data LayerPlot a = LayerMap (LayerMapFunc a)
                 | forall b . LayerFold (LayerFoldFunc a b) b

data Layer a = Layer
    { filename :: FilePath
    , trackNo :: TrackNo
    , dataLength :: Int
    , convEnee :: Enumeratee [Stream] [a] C.Render ()
    , plotter :: LayerPlot a
    }

data ScopeLayer = forall a . ScopeLayer (Layer a)

----------------------------------------------------------------------

data Scope = Scope
    { view   :: View
    , layers :: [ScopeLayer]
    }

data View = View
    { canvas :: G.DrawingArea
    , adj    :: G.Adjustment
    , viewX1 :: DataX
    , viewY1 :: Double
    , viewX2 :: DataX
    , viewY2 :: Double
    , dragDX :: Maybe DataX -- DataX of pointer at drag down
    }

scopeNew :: G.DrawingArea -> G.Adjustment -> Scope
scopeNew c adj = Scope {
      view = viewInit c adj
    , layers = []
    }

viewInit :: G.DrawingArea -> G.Adjustment -> View
viewInit c adj = View c adj (DataX 0.0) (-1.0) (DataX 1.0) 1.0 Nothing

----------------------------------------------------------------------
