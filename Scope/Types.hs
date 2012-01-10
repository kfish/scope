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

    , unionBounds
    , translateRange
    , unionRange
    , restrictRange
    , restrictRange01
    , zoomRange

    -- * Drawing commands
    , RGB
    , DrawCmd(..)
    , DrawLayer
    , ScopeRender(..)

    , ScopePlot(..)

    -- * Scope
    , Scope(..)
    , scopeNew
    , scopeUpdate
    , scopeModifyView

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
import Control.Monad.CatchIO
import Data.Time.Clock
import Data.Maybe
import Data.Iteratee (Enumeratee)
import Data.Offset
import Data.ZoomCache

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

instance Coordinate UTCTime where
    fromDouble d = addUTCTime (fromRational . toRational $ d) utc0
    toDouble u = fromRational . toRational $ diffUTCTime u utc0
    distance u1 u2 = fromDouble (distance (toDouble u1) (toDouble u2))
    translate t u = fromDouble (translate (toDouble t) (toDouble u))
    transform (Transform m b) x = fromDouble (transform (Transform m (toDouble b)) (toDouble x))

utc0 :: UTCTime
utc0 = UTCTime (toEnum 0) (fromInteger 0)

unionBounds :: Ord a => Maybe (a, a) -> Maybe (a, a) -> Maybe (a, a)
unionBounds a         Nothing   = a
unionBounds Nothing   b         = b
unionBounds (Just r1) (Just r2) = Just (unionRange r1 r2)

translateRange :: Coordinate a => a -> (a, a) -> (a, a)
translateRange t (x1, x2) = (translate t x1, translate t x2)

unionRange :: Ord a => (a, a) -> (a, a) -> (a, a)
unionRange (a1, a2) (b1, b2) = (min a1 b1, max a2 b2)

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

mkUTCDataTransform :: (UTCTime, UTCTime) -> (UTCTime, UTCTime) -> Transform DataX
mkUTCDataTransform (old1, old2) (new1, new2) = Transform m b
    where
        oldW = distance old1 old2
        newW = distance new1 new2
        m = toDouble oldW / toDouble newW
        b = fromDouble $ toDouble (distance new1 old1) / toDouble newW

----------------------------------------------------------------------

type RGB = (Double, Double, Double)

data DrawCmd =
      SetRGB   Double Double Double
    | SetRGBA  Double Double Double Double
    | MoveTo   (Double, Double)
    | LineTo   (Double, Double)
    | FillPoly [(Double, Double)]

----------------------------------------------------------------------

class (Functor m, MonadCatchIO m) => ScopeRender m where
    renderCmds :: [DrawCmd] -> m ()

----------------------------------------------------------------------

type DrawLayer = [DrawCmd]

-- | A layer plotting function which is just given the x position and x width
-- to render the data value of type 'a' into.
type LayerMapFunc a = Double -> Double -> a -> [DrawLayer]

-- | A layer plotting function which is given the x position and x width,
-- and a previously returned value of type 'b'
type LayerFoldFunc a b = Double -> Double -> b -> a -> ([DrawLayer], b)

data LayerPlot a = LayerMap (LayerMapFunc a) [DrawLayer]
                 | forall b . LayerFold (LayerFoldFunc a b) [DrawLayer] b

data Layer a = Layer
    { filename :: FilePath
    , layerTrackNo :: TrackNo
    , layerBaseUTC :: Maybe UTCTime
    , startTime :: TimeStamp
    , endTime :: TimeStamp
    , convEnee :: forall m . (Functor m, Monad m) => Enumeratee [Offset Block] [a] m ()
    , plotter :: LayerPlot a
    }

data ScopeLayer = forall a . Timestampable a => ScopeLayer (Layer a)

----------------------------------------------------------------------

class ScopePlot a where
    rawLayerPlot :: a -> RGB -> LayerPlot (TimeStamp, [a])
    summaryLayerPlot :: a -> RGB -> LayerPlot [Summary a]

----------------------------------------------------------------------

data Scope ui = Scope
    { view   :: View ui
    , bounds :: Maybe (TimeStamp, TimeStamp)
    , utcBounds :: Maybe (UTCTime, UTCTime)
    , layers :: [ScopeLayer]
    }

data View ui = View
    { viewX1 :: DataX
    , viewY1 :: Double
    , viewX2 :: DataX
    , viewY2 :: Double
    , pointerX :: Maybe CanvasX
    , dragDX :: Maybe DataX -- DataX of pointer at drag down
    , viewUI :: ui
    }

scopeNew :: ui -> Scope ui
scopeNew ui = Scope {
      view = viewInit ui
    , bounds = Nothing
    , utcBounds = Nothing
    , layers = []
    }

scopeModifyView :: (View ui -> View ui) -> Scope ui -> Scope ui
scopeModifyView f scope = scope{ view = f (view scope) }

scopeTransform :: Transform DataX -> Scope ui -> Scope ui
scopeTransform tf = scopeModifyView (viewTransform tf)

viewInit :: ui -> View ui
viewInit = View (DataX 0.0) (-1.0) (DataX 1.0) 1.0 Nothing Nothing

viewTransform :: Transform DataX -> View ui -> View ui
viewTransform tf v@View{..} = v {
      viewX1 = transform tf viewX1
    , viewX2 = transform tf viewX2
    , dragDX = transform tf <$> dragDX
    }

scopeUpdate :: Maybe (TimeStamp, TimeStamp)
            -> Maybe (UTCTime, UTCTime)
            -> Scope ui -> Scope ui
scopeUpdate newBounds Nothing scope =
    (t scope) { bounds = mb , utcBounds = Nothing }
    where
        oldBounds = bounds scope
        mb = unionBounds oldBounds newBounds
        t = case oldBounds of
                Just ob -> if oldBounds == mb
                               then id
                               else scopeTransform (mkTSDataTransform ob (fromJust mb))
                _ -> id

scopeUpdate newBounds (Just newUTCBounds) scope
    | (not . null . layers $ scope) && isNothing oldUTCBounds = scopeUpdate newBounds Nothing scope
    | otherwise = (t scope) { bounds = mb , utcBounds = umb }
    where
        oldBounds = bounds scope
        oldUTCBounds = utcBounds scope
        mb = unionBounds oldBounds newBounds
        umb = unionBounds oldUTCBounds (Just newUTCBounds)
        t = case oldUTCBounds of
                Just uob -> if oldUTCBounds == umb
                               then id
                               else scopeTransform (mkUTCDataTransform uob (fromJust umb))
                _ -> id

----------------------------------------------------------------------
