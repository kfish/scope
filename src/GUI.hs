{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
--
-- Based on Gtk2Hs/demo/cairo/Drawing2.hs 
-- Author: Johan Bockgård <bojohan@dd.chalmers.se>
--
-- This code is in the public domain.
--

module GUI (
    guiMain
) where

import Prelude hiding (catch)

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad (foldM, replicateM_)
import Control.Monad.CatchIO
import Control.Monad.Reader
import Data.Dynamic
import Data.IORef
import Data.Maybe
import Data.Iteratee (Iteratee, Enumeratee)
import qualified Data.Iteratee as I
import Data.ZoomCache.Numeric
import Data.ZoomCache.Texture
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo.Internal (Render(..))
import qualified Graphics.Rendering.Cairo.Matrix as M

import Text.Printf

import Paths_scope as My

import Debug.Trace

----------------------------------------------------------------------

class Coordinate a where
    fromDouble :: Double -> a
    toDouble :: a -> Double

    -- | Distance from to
    distance :: a -> a -> a
    -- | Translate x by
    translate :: a -> a -> a

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
    | w >= rW = trace "restrictPair: w >= rW" (rangeX1, rangeX2)
    | x1 < rangeX1 = trace "restrictPair: x1 < rangeX1" (rangeX1, translate rangeX1 w)
    | x2 > rangeX2 = trace "restrictPair: x2 > rangeX2" (x1', rangeX2)
    | otherwise = trace "restrictPair: otherwise" (x1, x2)
    where
        rW = distance rangeX1 rangeX2
        w = distance x1 x2
        x1' = distance w rangeX2

restrictPair01 :: (Ord a, Coordinate a) => (a, a) -> (a, a)
restrictPair01 = restrictPair (fromDouble 0.0, fromDouble 1.0)

-- zoomPair :: Coordinate a => Double -> (a, a) -> (a, a)
zoomPair :: (Show a, Coordinate a) => Double -> (a, a) -> (a, a)
zoomPair mult (x1, x2) = trace (
        printf "mult: %f negt: %s t: %s x1: %s x2: %s"
            mult (show negt) (show t) (show x1) (show x2)
    ) $ (translate negt x1, translate t x2)
    where
        negt = fromDouble $ negate t0
        t = fromDouble t0
        t0 = (newW - oldW) / 2
        oldW = toDouble $ distance x1 x2
        newW = min 1.0 (oldW * mult)

----------------------------------------------------------------------

type PlotLayer a = Double -> Double -> a -> C.Render ()

data Layer a = Layer
    { filename :: FilePath
    , trackNo :: TrackNo
    , dataLength :: Int
    , convEnee :: Enumeratee [Stream] [a] C.Render ()
    , plotValue :: PlotLayer a
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
    , dragCX  :: Maybe CanvasX -- canvas x coord of drag down
    , dragVX1 :: Maybe DataX -- viewX1 at time of drag down
    }

scopeNew :: G.DrawingArea -> G.Adjustment -> Scope
scopeNew c adj = Scope {
      view = viewInit c adj
    , layers = [ ScopeLayer textureLayer ]
    }

viewInit :: G.DrawingArea -> G.Adjustment -> View
viewInit c adj = View c adj (DataX 0.0) (-1.0) (DataX 1.0) 1.0 Nothing Nothing

----------------------------------------------------------------------

windowWidth, windowHeight :: Int
windowWidth   = 500
windowHeight  = 500

{-
-- Write image to file
_writePng :: IO ()
_writePng =
  C.withImageSurface C.FormatARGB32 width height $ \ result -> do
      C.renderWith result $ plotWindow width height
      C.surfaceWriteToPNG result "Draw.png"
  where width  = windowWidth
        height = windowHeight
-}

-- Display image in window
guiMain :: Chan String -> [String] -> IO ()
guiMain chan args = do
  _ <- G.initGUI

  window <- G.windowNew
  G.widgetSetSizeRequest window windowWidth windowHeight
  G.widgetSetAppPaintable window True
  G.widgetSetDoubleBuffered window True

  vbox <- G.vBoxNew False 0
  G.containerAdd window vbox

  ui <- G.uiManagerNew

  filename <- My.getDataFileName "data/actions.ui"
  G.uiManagerAddUiFromFile ui filename

  let getAction = fmap fromJust . G.uiManagerGetAction ui
      getWidget = fmap fromJust . G.uiManagerGetWidget ui

  -- Menubar
  fma <- G.actionNew "FMA" "File" Nothing Nothing
  ema <- G.actionNew "EMA" "Edit" Nothing Nothing
  vma <- G.actionNew "VMA" "View" Nothing Nothing
  hma <- G.actionNew "HMA" "Help" Nothing Nothing

  -- File menu
  newa <- G.actionNew "NEWA" "New" (Just "Just a Stub") (Just G.stockNew)
  newa `G.on` G.actionActivated $ myNew
  opena <- G.actionNew "OPENA" "Open" (Just "Just a Stub") (Just G.stockOpen)
  savea <- G.actionNew "SAVEA" "Save" (Just "Just a Stub") (Just G.stockSave)
  saveasa <- G.actionNew "SAVEASA" "Save As" (Just "Just a Stub") (Just G.stockSaveAs)
  quita <- G.actionNew "QUITA" "Quit" (Just "Just a Stub") (Just G.stockQuit)
  quita `G.on` G.actionActivated $ myQuit window chan

  let fChooser action label = G.fileChooserDialogNew Nothing (Just window) action
          [(G.stockCancel, G.ResponseCancel), (label, G.ResponseAccept)]

  openDialog <- fChooser G.FileChooserActionOpen G.stockOpen
  opena `G.on` G.actionActivated $ G.widgetShow openDialog
  openDialog `G.on` G.response $ myFileOpen openDialog

  saveDialog <- fChooser G.FileChooserActionSave G.stockSave
  savea `G.on` G.actionActivated $ G.widgetShow saveDialog
  saveasa `G.on` G.actionActivated $ G.widgetShow saveDialog
  saveDialog `G.on` G.response $ myFileSave saveDialog

  -- Edit menu
  cut1 <- G.actionNew "cut1" "Cut" (Just "Just a Stub") (Just G.stockCut)
  cut1 `G.on` G.actionActivated $ myCut
  copy1 <- G.actionNew "copy1" "Copy" (Just "Just a Stub") (Just G.stockCopy)
  copy1 `G.on` G.actionActivated $ myCopy
  paste1 <- G.actionNew "paste1" "Paste" (Just "Just a Stub") (Just G.stockPaste)
  paste1 `G.on` G.actionActivated $ myPaste
  delete1 <- G.actionNew "delete1" "Delete" (Just "Just a Stub") (Just G.stockDelete)
  delete1 `G.on` G.actionActivated $ myDelete

  -- Help menu
  -- About dialog
  aboutdialog <- G.aboutDialogNew
  abouta <- G.actionNew "ABOUTA" "About" (Just "Just a Stub") Nothing
  abouta `G.on` G.actionActivated $ G.widgetShow aboutdialog
  aboutdialog `G.on` G.response $ const $ G.widgetHide aboutdialog

  -- Action group
  agr <- G.actionGroupNew "AGR"
  mapM_ (G.actionGroupAddAction agr) [fma, ema, vma, hma]
  mapM_ (\act -> G.actionGroupAddActionWithAccel agr act Nothing)
      [ newa, opena, savea, saveasa, quita
      , cut1, copy1, paste1, delete1
      , abouta
      ]

  G.uiManagerInsertActionGroup ui agr 0

  menubar <- getWidget "/ui/menubar1"
  G.boxPackStart vbox menubar G.PackNatural 0

  adj <- G.adjustmentNew (0.0) (0.0) (1.0) (0.1) 1.0 1.0
  drawingArea <- G.drawingAreaNew

  scopeRef <- newIORef (scopeNew drawingArea adj)

  mapM_ (addLayersFromFile scopeRef) args

  adj `G.onValueChanged` (scroll scopeRef)

  G.boxPackStart vbox drawingArea G.PackGrow 0

  drawingArea `G.on` G.buttonPressEvent $ G.tryEvent $ buttonDown scopeRef
  drawingArea `G.on` G.buttonReleaseEvent $ G.tryEvent $ buttonRelease scopeRef
  drawingArea `G.on` G.scrollEvent $ G.tryEvent $ wheel scopeRef
  drawingArea `G.on` G.motionNotifyEvent $ G.tryEvent $ motion scopeRef
  drawingArea `G.on` G.keyPressEvent $ G.tryEvent $ do
      liftIO $ putStrLn "Key pressed"
  G.widgetAddEvents drawingArea
    [ G.KeyPressMask
    , G.KeyReleaseMask
    -- , G.PointerMotionMask
    , G.Button1MotionMask
    , G.ScrollMask
    ]

  -- _ <- G.onExpose drawingArea $ const (updateCanvas drawingArea)
  cid <- drawingArea `G.on` G.exposeEvent $ G.tryEvent $ do
    liftIO $ updateCanvas scopeRef
    return ()

  scrollbar <- G.hScrollbarNew adj
  G.boxPackStart vbox scrollbar G.PackNatural 0

  statusbar <- G.statusbarNew
  G.boxPackStart vbox statusbar G.PackNatural 0

  G.onDestroy window ((myWriteChan chan "quit") >> G.mainQuit)

  G.widgetShowAll window
  G.mainGUI

myQuit :: G.WidgetClass cls => cls -> Chan String -> IO ()
myQuit window chan = do
  G.widgetDestroy window
  myWriteChan chan "quit"

myWriteChan :: Chan String -> String -> IO ()
myWriteChan chan s = do writeChan chan s
                        yield
myNew :: IO ()
myNew = putStrLn "New"

myFileOpen :: G.FileChooserDialog -> G.ResponseId -> IO ()
myFileOpen fcdialog response = do
  case response of
    G.ResponseOk -> do Just filename <- G.fileChooserGetFilename fcdialog
                       putStrLn filename
    G.ResponseCancel -> putStrLn "Cancelled!"
    G.ResponseDeleteEvent -> putStrLn "FileChooserDialog Deleted!"
    G.ResponseClose -> putStrLn "Closed!"
    _ -> return ()
  G.widgetHide fcdialog

myFileSave :: G.FileChooserDialog -> G.ResponseId -> IO ()
myFileSave = myFileOpen

myCut :: IO ()
myCut = putStrLn "Cut"

myCopy :: IO ()
myCopy = putStrLn "Copy"

myPaste :: IO ()
myPaste = putStrLn "Paste"

myDelete :: IO ()
myDelete = putStrLn "Delete"

updateCanvas :: IORef Scope -> IO Bool
updateCanvas ref = do
    scope <- readIORef ref
    let c = canvas . view $ scope
    win <- G.widgetGetDrawWindow c
    (width, height) <- G.widgetGetSize c
    G.renderWithDrawable win $ plotWindow width height scope
    return True

----------------------------------------------------------------

addLayersFromFile :: IORef Scope -> FilePath -> IO ()
addLayersFromFile ref path = do
    scope <- readIORef ref
    let layers' = layers scope ++ layersFromFile path
        scope' = scope { layers = layers' }
    writeIORef ref scope'

----------------------------------------------------------------

viewSetEnds :: DataX -> DataX -> View -> IO View
viewSetEnds x1 x2 v@View{..} = do
    -- putStrLn $ printf "setEnds %f %f\n" x1 x2
    return view'
    where
      view' = v { viewX1 = x1, viewX2 = x2 }
      -- w = min 1.0 (x2 - x1)

scopeUpdate :: IORef Scope -> Scope -> IO ()
scopeUpdate ref scope = do
    writeIORef ref scope
    let View{..} = view scope
    G.adjustmentSetValue adj (toDouble viewX1)
    G.adjustmentSetPageSize adj $ toDouble (distance viewX1 viewX2)
    G.widgetQueueDraw canvas

----------------------------------------------------------------

canvasToScreen :: G.DrawingArea -> CanvasX -> IO ScreenX
canvasToScreen c (CanvasX cX) = do
    (width, _height) <- G.widgetGetSize c
    return $ ScreenX (fromIntegral width * cX)

screenToCanvas :: G.DrawingArea -> ScreenX -> IO CanvasX
screenToCanvas c (ScreenX sX) = do
    (width, _height) <- G.widgetGetSize c
    return $ CanvasX (sX / fromIntegral width)

canvasToData :: View -> CanvasX -> DataX
canvasToData View{..} (CanvasX cX) = translate viewX1 $
    DataX (cX * toDouble (distance viewX1 viewX2))

buttonDown :: IORef Scope -> G.EventM G.EButton ()
buttonDown ref = do
    (x, y) <- G.eventCoordinates
    liftIO $ do
        scope <- readIORef ref
        let c = canvas . view $ scope
        putStrLn $ printf "down (%f, %f)" x y
        cX <- screenToCanvas c (ScreenX x)
        let dX = canvasToData (view scope) cX
        -- dX <- canvasToData (view scope) <$> screenToCanvas c x
        -- putStrLn $ printf "down SCREEN %f DATA %f" cX dX
        let v = view scope
            view' = v { dragCX = Just cX
                      , dragVX1 = Just (viewX1 v)
                      }
            scope' = scope { view = view' }
        writeIORef ref scope'

buttonRelease :: IORef Scope -> G.EventM G.EButton ()
buttonRelease ref = do
    (x, y) <- G.eventCoordinates
    liftIO $ do
        scope <- readIORef ref
        putStrLn $ printf "release (%f, %f)" x y
        let view' = (view scope) { dragCX = Nothing
                                 , dragVX1 = Nothing }
            scope' = scope { view = view' }
        writeIORef ref scope'

motion :: IORef Scope -> G.EventM G.EMotion ()
motion ref = do
    (x, y) <- G.eventCoordinates
    liftIO $ do
        scope <- readIORef ref
        let v@View{..} = view scope
        cX <- screenToCanvas canvas (ScreenX x)
        let dX = canvasToData v cX
        let cX0 = fromJust dragCX
            vX1 = fromJust dragVX1
        putStrLn $ printf "motion (%f, %f)" x y
        -- putStrLn $ printf "motion %f -> %f" cX0 cX
        -- let newX1 = max 0 $ vX1 + (cX0 - cX)
        --     newX2 = newX1 + (viewX2 - viewX1)

        let (newX1, newX2) = restrictPair01 .
                translatePair (fromDouble (toDouble (distance cX cX0))) $
                (viewX1, viewX2)
        view' <- viewSetEnds newX1 newX2 v
        let scope' = scope { view = view'}
        scopeUpdate ref scope'

wheel :: IORef Scope -> G.EventM G.EScroll ()
wheel ref = do
    dir <- G.eventScrollDirection
    liftIO $ do
        scope <- readIORef ref
        let mult = case dir of
                       G.ScrollUp   -> 0.9
                       G.ScrollDown -> 1.1111
                       _            -> 1.0
            v@View{..} = view scope
            (newX1, newX2') = restrictPair01 $
                zoomPair mult (viewX1, viewX2)
{-
            oldW = viewX2 - viewX1
            newW = min 1.0 (oldW * mult)
            newX2 = if viewX2 >= 0.99999
                        then 1.0
                        else viewX2 - (oldW - newW)/2
            (newX1, newX2') = if viewX1 == 0.0
                                  then (0.0, newW)
                                  else (newX2 - newW, newX2)
-}
            
        -- putStrLn $ printf "WHEEL mult %f newW %f" mult newW
        view' <- viewSetEnds newX1 newX2' v
        let scope' = scope { view = view'}
        scopeUpdate ref scope'

scroll :: IORef Scope -> IO ()
scroll ref = do
    scope <- readIORef ref
    val <- G.adjustmentGetValue (adj . view $ scope)
    putStrLn $ printf "SCROLL %f" val

    let v@View{..} = view scope
        newX1', newX2' :: DataX
        (newX1', newX2') = restrictPair01 .
            translatePair (distance viewX1 (DataX val)) $
            (viewX1, viewX2)
{-
        oldW = distance viewX1 viewX2
        newX1 = if val < 0.0
                    then DataX 0.0
                    else DataX val
        newX2 = translate newX1 oldW
        newX1', newX2' :: DataX
        (newX1', newX2') = if newX2 > 1.0
                              then (1.0 - oldW, 1.0)
                              else (newX1, newX2)
-}
               
    view' <- viewSetEnds newX1' newX2' v
    let scope' = scope { view = view'}
    scopeUpdate ref scope'

----------------------------------------------------------------

foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

keepState :: C.Render t -> C.Render ()
keepState render = do
  C.save
  _ <- render
  C.restore

----------------------------------------------------------------

plotWindow :: Int -> Int -> Scope -> C.Render ()
plotWindow width height scope = do
    prologue width height (view scope)
    plotLayers scope

-- Set up stuff
prologue :: Int -> Int -> View -> C.Render ()
prologue wWidth wHeight View{..} = do
  -- Define viewport coords as (-1.0, -1.0) - (1.0, 1.0)
  let width   = 1.0
      height  = 2.0
      xmax    = 1.0
      xmin    = 0.0
      ymax    = 1.0
      ymin    = -1.0
      scaleX  = realToFrac wWidth  / width
      scaleY  = realToFrac wHeight / height

  -- style and color
  C.setLineCap C.LineCapRound
  C.setLineJoin C.LineJoinRound
  C.setLineWidth $ 1 / max scaleX scaleY
  C.setSourceRGBA 0.5 0.7 0.5 0.5

  -- Set up user coordinates
  C.scale scaleX scaleY
  -- center origin vertically
  C.translate 0 (height / 2)
  -- positive y-axis upwards
  let flipY = M.Matrix 1 0 0 (-1) 0 0
  C.transform flipY

  grid xmin xmax ymin ymax


-- Grid and axes
grid :: Double -> Double -> Double -> Double -> C.Render ()
grid xmin xmax ymin ymax =
  keepState $ do
  C.setSourceRGBA 0 0 0 0.7
  -- axes
  C.moveTo 0 ymin; C.lineTo 0 ymax; C.stroke
  C.moveTo xmin 0; C.lineTo xmax 0; C.stroke
  -- grid
  C.setDash [0.01, 0.99] 0
  foreach [xmin .. xmax] $ \ x ->
      do C.moveTo x ymin
         C.lineTo x ymax
         C.stroke

----------------------------------------------------------------

instance MonadCatchIO C.Render where
  m `catch` f = mapRender (\m' -> m' `catch` \e -> runRender $ f e) m
  block       = mapRender block
  unblock     = mapRender unblock

mapRender f = Render . f . runRender

----------------------------------------------------------------

plotLayers :: Scope -> C.Render ()
plotLayers scope = keepState $ mapM_ (plotLayer scope) (layers scope)

plotLayer :: Scope -> ScopeLayer -> C.Render ()
plotLayer scope (ScopeLayer Layer{..}) = keepState $ do
    C.setSourceRGB 0.0 0 1.0
    I.fileDriverRandom (I.joinI $
        enumCacheFile identifiers (I.joinI . filterTracks [trackNo] . I.joinI . convEnee $ foldData)
        ) filename
    where
        View{..} = view scope

        foldData = do
            I.drop (skipLength dataLength)
            I.joinI . I.take (visibleLength dataLength) $ canvasMap

        identifiers = standardIdentifiers ++ textureIdentifiers

        canvasMap = do
            I.foldM (render plotValue (stepWidth dataLength)) (canvasX0 dataLength) >> return ()
            lift $ C.stroke

        -- | Canvas X coordinate of first data point
        canvasX0 :: Int -> Double
        canvasX0 _ = 0.0 

        render :: PlotLayer a -> Double -> Double -> a -> C.Render Double
        render plot w x d = do
            plot x w d
            return (x + w)

        -- | Count of data points to drop before rendering
        skipLength :: Int -> Int
        skipLength l = floor $ fromIntegral l * toDouble viewX1

        -- | Count of data points visible in view
        visibleLength :: Int -> Int
        visibleLength l = ceiling (viz l) + 1

        -- | Canvas x length per data point
        stepWidth s = 1.0 / (toDouble (distance viewX1 viewX2) * fromIntegral s) -- stepWidth l = 1.0 / viz l

        -- | Fractional number of data points visible in view
        viz l = fromIntegral l * toDouble (distance viewX1 viewX2)

----------------------------------------------------------------------
-- Texture

textureLayer :: Layer (TimeStamp, TextureSlice)
textureLayer = Layer texturePath 1 textureSize enumTexture renderTex
    where
        texturePath = "../texture-synthesis/texture.zoom"
        textureSize = (2^5)+1
        texH = 2.0 {- (viewY2 v - viewY1 v)-} / fromIntegral textureSize

        renderTex :: Double -> Double -> (TimeStamp, TextureSlice) -> C.Render ()
        renderTex x w (_ts, (TextureSlice tex)) = do
            mapM_ (uncurry (texVal x w)) (zip (iterate (+texH) (-1.0 {- (viewY1 v) -})) tex)

        texVal :: Double -> Double -> Double -> Float -> C.Render ()
        texVal x w y v = do
            C.setSourceRGB s (s*0.9) (s-0.03)
            C.rectangle x y (w+0.01) (texH+0.01)
            C.fill
            where
	        s = (realToFrac v / 4) + 0.75

----------------------------------------------------------------------
-- Raw data

plotRaw :: Double -> Double -> Double -> (TimeStamp, Double) -> C.Render ()
plotRaw yR x _ (_ts, y) = do
    C.lineTo x (y * 2.0 {- (viewY2 v - viewY1 v)-} / yR)

----------------------------------------------------------------------
-- Summary data

plotSummary :: Double -> Double -> Double -> Double -> PlotLayer (Summary Double)
plotSummary dYRange r g b x _ s = do
    C.setSourceRGB r g b
    C.lineTo x y
    where
        y = fx s * 4.0 / dYRange
        fx = numMax . summaryData

----------------------------------------------------------------------

layersFromFile :: FilePath -> [ScopeLayer]
layersFromFile dataPath = [ ScopeLayer rawTrack1, ScopeLayer rawTrack2
                          , ScopeLayer summaryTrack1, ScopeLayer summaryTrack2
                          ]
    where
        rawTrack1 :: Layer (TimeStamp, Double)
        rawTrack1 = Layer dataPath 1 5000 enumDouble (plotRaw 1000000000.0)

        rawTrack2 :: Layer (TimeStamp, Double)
        rawTrack2 = Layer dataPath 2 5000 enumDouble (plotRaw 300000.0)

        summaryTrack1 :: Layer (Summary Double)
        summaryTrack1 = Layer dataPath 1 20 (enumSummaryDouble 1) (plotSummary 1000000000.0 1.0 0 0)

        summaryTrack2 :: Layer (Summary Double)
        summaryTrack2 = Layer dataPath 2 20 (enumSummaryDouble 1) (plotSummary 300000.0 1.0 0 0)
