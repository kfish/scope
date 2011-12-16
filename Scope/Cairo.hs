{-# OPTIONS -Wall -fno-warn-orphans #-}

module Scope.Cairo (
    -- * Types
      ViewCairo(..)

    -- * Scope ViewCairo
    , scopeCairoNew
    , viewCairoInit

    -- * Utils
    , keepState
) where

import Prelude hiding (catch)

import Control.Monad.CatchIO
import Control.Monad.Reader
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo.Internal (Render(..))
import Graphics.Rendering.Cairo.Types (Cairo)
import qualified Graphics.UI.Gtk as G

import Scope.Types hiding (m, b)

----------------------------------------------------------------------

data ViewCairo = ViewCairo
    { canvas :: G.DrawingArea
    , adj    :: G.Adjustment
    }

scopeCairoNew :: G.DrawingArea -> G.Adjustment -> Scope ViewCairo
scopeCairoNew c a = scopeNew (viewCairoInit c a)

viewCairoInit :: G.DrawingArea -> G.Adjustment -> ViewCairo
viewCairoInit c a = ViewCairo c a

----------------------------------------------------------------------

instance MonadCatchIO C.Render where
  m `catch` f = mapRender (\m' -> m' `catch` \e -> runRender $ f e) m
  block       = mapRender block
  unblock     = mapRender unblock

mapRender :: (ReaderT Cairo IO m1 -> ReaderT Cairo IO m) -> Render m1 -> Render m
mapRender f = Render . f . runRender

instance ScopeRender C.Render where
    renderCmds = keepState . mapM_ cairoDrawCmd

----------------------------------------------------------------------

cairoDrawCmd :: DrawCmd -> C.Render ()
cairoDrawCmd (SetRGB  r g b)   = C.setSourceRGB  r g b
cairoDrawCmd (SetRGBA r g b a) = C.setSourceRGBA r g b a
cairoDrawCmd (MoveTo (x,y))    = C.moveTo x y

cairoDrawCmd (LineTo (x,y))    = do
    C.lineTo x y
    C.stroke

cairoDrawCmd (FillPoly [])         = return ()
cairoDrawCmd (FillPoly ((x,y):ps)) = do
    C.moveTo x y
    mapM_ (uncurry C.lineTo) ps
    C.fill

----------------------------------------------------------------

keepState :: C.Render t -> C.Render ()
keepState render = do
  C.save
  _ <- render
  C.restore

