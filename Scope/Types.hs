{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
{- |
   Module      : Scope.Types
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

   Scope types and interfaces

   The coordinate system:

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

    , Transform(..)
    , mkTransform
    , mkTSDataTransform

    , restrictRange
    , restrictRange01
    , translateRange
    , zoomRange

    -- * Scope
    , Scope(..)
    , scopeNew
    , scopeTransform

    -- * Views
    , View(..)

    -- * Layers
    , Layer(..)
    , LayerPlot(..)
    , LayerMapFunc
    , LayerFoldFunc
    , ScopeLayer(..)
) where

import Control.Applicative ((<$>))
import Data.Maybe
import Data.Iteratee (Enumeratee)
import Data.ZoomCache

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk as G

----------------------------------------------------------------------

data Transform a = Transform { m :: Double, b :: a }

class Coordinate a where
    fromDouble :: Double -> a
    toDouble :: a -> Double

    -- | Distance from to
    distance :: a -> a -> a
    -- | Translate x by
    translate :: a -> a -> a

    transform :: Transform a -> a -> a

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
    transform Transform{..} x = m * x + b

instance Coordinate ScreenX where
    fromDouble d = ScreenX d
    toDouble (ScreenX d) = d
    distance (ScreenX x1) (ScreenX x2) = ScreenX (distance x1 x2)
    translate (ScreenX t) (ScreenX x)  = ScreenX (translate t x)
    transform (Transform m (ScreenX b)) (ScreenX x) = ScreenX (transform (Transform m b) x)

instance Coordinate CanvasX where
    fromDouble d = CanvasX d
    toDouble (CanvasX d) = d
    distance (CanvasX x1) (CanvasX x2) = CanvasX (distance x1 x2)
    translate (CanvasX t) (CanvasX x)  = CanvasX (translate t x)
    transform (Transform m (CanvasX b)) (CanvasX x) = CanvasX (transform (Transform m b) x)

instance Coordinate DataX where
    fromDouble d = DataX d
    toDouble (DataX d) = d
    distance (DataX x1) (DataX x2) = DataX (distance x1 x2)
    translate (DataX t) (DataX x)  = DataX (translate t x)
    transform (Transform m (DataX b)) (DataX x) = DataX (transform (Transform m b) x)

instance Coordinate TimeStamp where
    fromDouble d = TS d
    toDouble (TS d) = d
    distance (TS x1) (TS x2) = TS (distance x1 x2)
    translate (TS t) (TS x)  = TS (translate t x)
    transform (Transform m (TS b)) (TS x) = TS (transform (Transform m b) x)

translateRange :: Coordinate a => a -> (a, a) -> (a, a)
translateRange t (x1, x2) = (translate t x1, translate t x2)

-- | Restrict a window to within a given range
restrictRange :: (Ord a, Coordinate a) => (a, a) -> (a, a) -> (a, a)
restrictRange (rangeX1, rangeX2) (x1, x2)
    | w >= rW      = (rangeX1, rangeX2)
    | x1 < rangeX1 = (rangeX1, translate rangeX1 w)
    | x2 > rangeX2 = (x1', rangeX2)
    | otherwise    = (x1, x2)
    where
        rW = distance rangeX1 rangeX2
        w = distance x1 x2
        x1' = distance w rangeX2

restrictRange01 :: (Ord a, Coordinate a) => (a, a) -> (a, a)
restrictRange01 = restrictRange (fromDouble 0.0, fromDouble 1.0)

zoomRange :: Coordinate a => CanvasX -> Double -> (a, a) -> (a, a)
zoomRange (CanvasX focus) mult (x1, x2) = (translate off1 x1, translate off2 x2)
    where
        off1 = fromDouble $ (oldW - newW) * focus
        off2 = fromDouble $ (newW - oldW) * (1.0 - focus)
        oldW = toDouble $ distance x1 x2
        newW = min 1.0 (oldW * mult)

mkTransform :: Coordinate a => (a, a) -> (a, a) -> Transform a
mkTransform (old1, old2) (new1, new2) = Transform m b
    where
        oldW = distance old1 old2
        newW = distance new1 new2
        m = toDouble oldW / toDouble newW
        b = distance new1 old1

mkTSDataTransform :: (TimeStamp, TimeStamp) -> (TimeStamp, TimeStamp) -> Transform DataX
mkTSDataTransform (old1, old2) (new1, new2) = Transform m b
    where
        oldW = distance old1 old2
        newW = distance new1 new2
        m = toDouble oldW / toDouble newW
        b = fromDouble $ toDouble (distance new1 old1) / toDouble newW

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
    , startTime :: TimeStamp
    , endTime :: TimeStamp
    , convEnee :: Enumeratee [Stream] [a] C.Render ()
    , plotter :: LayerPlot a
    }

data ScopeLayer = forall a . Timestampable a => ScopeLayer (Layer a)

----------------------------------------------------------------------

data Scope = Scope
    { view   :: View
    , bounds :: Maybe (TimeStamp, TimeStamp)
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
    , bounds = Nothing
    , layers = []
    }

scopeTransform :: Transform DataX -> Scope -> Scope
scopeTransform tf scope@Scope{..} = scope { view = viewTransform tf view }

viewInit :: G.DrawingArea -> G.Adjustment -> View
viewInit c adj = View c adj (DataX 0.0) (-1.0) (DataX 1.0) 1.0 Nothing

viewTransform :: Transform DataX -> View -> View
viewTransform tf v@View{..} = v {
      viewX1 = transform tf viewX1
    , viewX2 = transform tf viewX2
    , dragDX = transform tf <$> dragDX
    }

----------------------------------------------------------------------
