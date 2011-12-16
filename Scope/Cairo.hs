{-# OPTIONS -Wall #-}

module Scope.Cairo (
    -- * Types
      ViewCairo(..)

    -- * Scope ViewCairo
    , scopeCairoNew
    , viewCairoInit
) where

import qualified Graphics.UI.Gtk as G

import Scope.Types

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


