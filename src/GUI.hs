{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind -fno-warn-orphans #-}
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
import Control.Monad.CatchIO
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import qualified Data.Iteratee as I
import Data.ZoomCache.Numeric
import Data.ZoomCache.Texture
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo.Internal (Render(..))
import Graphics.Rendering.Cairo.Types (Cairo)
import qualified Graphics.Rendering.Cairo.Matrix as M

import Text.Printf

import Paths_scope as My
import Scope.Types

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

  let getWidget = fmap fromJust . G.uiManagerGetWidget ui

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

  let scope = scopeNew drawingArea adj
  scopeRef <- newIORef (scope { layers = [ScopeLayer textureLayer] })

  mapM_ (modifyIORef scopeRef . addLayersFromFile) args
  openDialog `G.on` G.response $ myFileOpen scopeRef openDialog

  adj `G.onValueChanged` (scroll scopeRef)

  G.boxPackStart vbox drawingArea G.PackGrow 0

  drawingArea `G.on` G.buttonPressEvent $ G.tryEvent $ buttonDown scopeRef
  drawingArea `G.on` G.buttonReleaseEvent $ G.tryEvent $ buttonRelease scopeRef
  drawingArea `G.on` G.scrollEvent $ G.tryEvent $ wheel scopeRef
  drawingArea `G.on` G.motionNotifyEvent $ G.tryEvent $ motion scopeRef
  drawingArea `G.on` G.keyPressEvent $ G.tryEvent $ keyDown scopeRef
  G.widgetAddEvents drawingArea
    [ G.KeyPressMask
    , G.KeyReleaseMask
    -- , G.PointerMotionMask
    , G.ButtonMotionMask
    , G.ScrollMask
    ]

  G.widgetSetCanFocus drawingArea True

  drawingArea `G.on` G.exposeEvent $ G.tryEvent $ do
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

myFileOpen :: IORef Scope -> G.FileChooserDialog -> G.ResponseId -> IO ()
myFileOpen scopeRef fcdialog response = do
  case response of
    G.ResponseAccept -> do
        Just filename <- G.fileChooserGetFilename fcdialog
        scopeModifyRedraw scopeRef (addLayersFromFile filename)
    _ -> return ()
  G.widgetHide fcdialog

myFileSave :: G.FileChooserDialog -> G.ResponseId -> IO ()
myFileSave _ _ = return ()

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

scopeAlign :: CanvasX -> DataX -> IORef Scope -> IO ()
scopeAlign cx dx ref = do
    scope <- readIORef ref
    let scope' = scope { view = viewAlign cx dx (view scope) }
    scopeUpdate ref scope'

scopeMoveLeft :: IORef Scope -> IO ()
scopeMoveLeft ref = do
    scope <- readIORef ref
    let View{..} = view scope
    scopeAlign (CanvasX 0.0) viewX2 ref

scopeMoveRight :: IORef Scope -> IO ()
scopeMoveRight ref = do
    scope <- readIORef ref
    let View{..} = view scope
    scopeAlign (CanvasX 1.0) viewX1 ref

----------------------------------------------------------------

scopeZoomIn :: Double -> IORef Scope -> IO ()
scopeZoomIn = scopeZoomInOn (CanvasX 0.5)

scopeZoomOut :: Double -> IORef Scope -> IO ()
scopeZoomOut = scopeZoomOutOn (CanvasX 0.5)

scopeZoomInOn :: CanvasX -> Double -> IORef Scope -> IO ()
scopeZoomInOn focus mult = scopeZoomOutOn focus (1.0/mult)

scopeZoomOutOn :: CanvasX -> Double -> IORef Scope -> IO ()
scopeZoomOutOn focus mult ref = do
    scope <- readIORef ref
    let v@View{..} = view scope
        (newX1, newX2') = restrictPair01 $
            zoomPair focus mult (viewX1, viewX2)
        scope' = scope { view = viewSetEnds newX1 newX2' v }
    scopeUpdate ref scope'

scopeModifyRedraw :: IORef Scope -> (Scope -> Scope) -> IO ()
scopeModifyRedraw ref f = do
    modifyIORef ref f
    G.widgetQueueDraw =<< canvas . view <$> readIORef ref

scopeModifyUpdate :: IORef Scope -> (Scope -> Scope) -> IO ()
scopeModifyUpdate ref f = do
    modifyIORef ref f
    View{..} <- view <$> readIORef ref
    G.adjustmentSetValue adj (toDouble viewX1)
    G.adjustmentSetPageSize adj $ toDouble (distance viewX1 viewX2)
    G.widgetQueueDraw canvas

scopeUpdate :: IORef Scope -> Scope -> IO ()
scopeUpdate ref scope = do
    writeIORef ref scope
    let View{..} = view scope
    G.adjustmentSetValue adj (toDouble viewX1)
    G.adjustmentSetPageSize adj $ toDouble (distance viewX1 viewX2)
    G.widgetQueueDraw canvas

----------------------------------------------------------------

_canvasToScreen :: G.DrawingArea -> CanvasX -> IO ScreenX
_canvasToScreen c (CanvasX cX) = do
    (width, _height) <- G.widgetGetSize c
    return $ ScreenX (fromIntegral width * cX)

screenToCanvas :: G.DrawingArea -> ScreenX -> IO CanvasX
screenToCanvas c (ScreenX sX) = do
    (width, _height) <- G.widgetGetSize c
    return $ CanvasX (sX / fromIntegral width)

canvasToData :: View -> CanvasX -> DataX
canvasToData View{..} (CanvasX cX) = translate viewX1 $
    DataX (cX * toDouble (distance viewX1 viewX2))

----------------------------------------------------------------

buttonDown :: IORef Scope -> G.EventM G.EButton ()
buttonDown ref = do
    (x, _y) <- G.eventCoordinates
    liftIO $ do
        c <- canvas . view <$> readIORef ref
        cX <- screenToCanvas c (ScreenX x)
        modifyIORef ref (scopeModifyView (viewButtonDown cX))

buttonRelease :: IORef Scope -> G.EventM G.EButton ()
buttonRelease ref = liftIO $ modifyIORef ref (scopeModifyView viewButtonRelease)

motion :: IORef Scope -> G.EventM G.EMotion ()
motion ref = do
    (x, _y) <- G.eventCoordinates
    liftIO $ do
        View{..} <- view <$> readIORef ref
        cX <- screenToCanvas canvas (ScreenX x)
        scopeModifyUpdate ref $ scopeModifyView (viewButtonMotion cX)

wheel :: IORef Scope -> G.EventM G.EScroll ()
wheel ref = do
    (x, _y) <- G.eventCoordinates
    dir <- G.eventScrollDirection
    liftIO $ do
        scope <- readIORef ref
        let View{..} = view scope
        cX <- screenToCanvas canvas (ScreenX x)
        case dir of
            G.ScrollUp   -> scopeZoomInOn  cX 1.2 ref
            G.ScrollDown -> scopeZoomOutOn cX 1.2 ref
            _            -> return ()

scroll :: IORef Scope -> IO ()
scroll ref = do
    val <- G.adjustmentGetValue =<< adj . view <$> readIORef ref
    scopeModifyUpdate ref $ scopeModifyView (viewMoveTo val)

----------------------------------------------------------------

-- Some keys we are interested in, from:
-- http://cgit.freedesktop.org/xorg/proto/x11proto/plain/keysymdef.h
#define XK_Home                          0xff50
#define XK_Left                          0xff51  /* Move left, left arrow */
#define XK_Up                            0xff52  /* Move up, up arrow */
#define XK_Right                         0xff53  /* Move right, right arrow */
#define XK_Down                          0xff54  /* Move down, down arrow */
#define XK_Page_Up                       0xff55
#define XK_Page_Down                     0xff56
#define XK_End                           0xff57  /* EOL */

keyDown :: IORef Scope -> G.EventM G.EKey ()
keyDown ref = do
    n <- G.eventKeyName
    v <- G.eventKeyVal
    liftIO . putStrLn $ printf "Key %s (%d) pressed" n v
    liftIO $ case v of
        XK_Home -> scopeAlign (CanvasX 0.0) (DataX 0.0) ref
        XK_End  -> scopeAlign (CanvasX 1.0) (DataX 1.0) ref
        XK_Up   -> scopeZoomIn  2.0 ref
        XK_Down -> scopeZoomOut 2.0 ref
        XK_Left  -> scopeMoveRight ref
        XK_Right -> scopeMoveLeft ref
        XK_Page_Up -> putStrLn "XK_PageUp"
        _ -> putStrLn "Random key"

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

mapRender :: (ReaderT Cairo IO m1 -> ReaderT Cairo IO m) -> Render m1 -> Render m
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
            I.drop skipLength
            I.joinI . I.take visibleLength $ render plotter

        identifiers = standardIdentifiers ++ textureIdentifiers

        render (LayerMap f) = do
            I.foldM renderMap canvasX0 >> return ()
            lift $ C.stroke
            where
                renderMap x d = do
                    f x stepWidth d
                    return (x + stepWidth)
        render (LayerFold f b00) = do
            I.foldM renderFold (canvasX0, b00) >> return ()
            lift $ C.stroke
            where
                renderFold (x, b0) d = do
                    b <- f x stepWidth b0 d
                    return (x + stepWidth, b)

        -- | Canvas X coordinate of first data point
        canvasX0 = (fromIntegral skipLength - skip) * stepWidth

        -- | Count of data points to drop before rendering
        skipLength = floor skip

        -- | DataX coordinate of start of view
        skip = fromIntegral dataLength * toDouble viewX1

        -- | Count of data points visible in view
        visibleLength = ceiling viz + 2

        -- | Canvas x length per data point
        stepWidth = 1.0 / viz

        -- | Fractional number of data points visible in view
        viz = fromIntegral dataLength * toDouble (distance viewX1 viewX2)

----------------------------------------------------------------------
-- Texture

textureLayer :: Layer (TimeStamp, TextureSlice)
textureLayer = Layer texturePath 1 textureSize enumTexture (LayerMap renderTex)
    where
        texturePath = "../texture-synthesis/texture.zoom"
        textureSize = (2^(5::Int))+1
        texH = 2.0 {- (viewY2 v - viewY1 v)-} / fromIntegral textureSize

        renderTex :: Double -> Double -> (TimeStamp, TextureSlice) -> C.Render ()
        renderTex x w (_ts, (TextureSlice tex)) = do
            mapM_ (uncurry (texVal x w)) (zip (iterate (+texH) (-1.0 {- (viewY1 v) -})) tex)

        texVal :: Double -> Double -> Double -> Float -> C.Render ()
        texVal x w y v = do
            C.setSourceRGB s s (s*0.9)
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

plotSummary :: Double -> Double -> Double -> Double
            -> LayerFoldFunc (Summary Double) (Maybe (Summary Double))
plotSummary dYRange r g b x w Nothing s =
    plotSummary dYRange r g b x w (Just s) s
plotSummary dYRange r g b x w (Just s0) s = do
    C.setSourceRGBA r g b 0.3
    C.moveTo x     (y (numMax sd0))
    C.lineTo (x+w) (y (numMax sd))
    C.lineTo (x+w) (y (numMin sd))
    C.lineTo x     (y (numMin sd0))
    C.fill

    C.setSourceRGB (r*0.6) (g*0.6) (b*0.6)
    C.moveTo x     (y (numAvg sd0))
    C.lineTo (x+w) (y (numAvg sd))
    C.stroke
    return (Just s)
    where
        sd0 = summaryData s0
        sd = summaryData s
        y v = v * 4.0 / dYRange

----------------------------------------------------------------------

scopeModifyView :: (View -> View) -> Scope -> Scope
scopeModifyView f scope = scope{ view = f (view scope) }

----------------------------------------------------------------------

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

viewButtonDown :: CanvasX -> View -> View
viewButtonDown cX v = v { dragDX = Just (canvasToData v cX) }

viewButtonMotion :: CanvasX -> View -> View
viewButtonMotion cX v@View{..} = viewAlign cX (fromJust dragDX) v

viewButtonRelease :: View -> View
viewButtonRelease v = v { dragDX = Nothing}

----------------------------------------------------------------------

layersFromFile :: FilePath -> [ScopeLayer]
layersFromFile dataPath = [ ScopeLayer rawTrack1, ScopeLayer rawTrack2
                          , ScopeLayer summaryTrack1, ScopeLayer summaryTrack2
                          ]
    where
        rawTrack1 :: Layer (TimeStamp, Double)
        rawTrack1 = Layer dataPath 1 5000 enumDouble (LayerMap $ plotRaw 1000000000.0)

        rawTrack2 :: Layer (TimeStamp, Double)
        rawTrack2 = Layer dataPath 2 5000 enumDouble (LayerMap $ plotRaw 300000.0)

        summaryTrack1 :: Layer (Summary Double)
        summaryTrack1 = Layer dataPath 1 20 (enumSummaryDouble 1)
                            (LayerFold (plotSummary 1000000000.0 1.0 0 0) Nothing)

        summaryTrack2 :: Layer (Summary Double)
        summaryTrack2 = Layer dataPath 2 20 (enumSummaryDouble 1)
                            (LayerFold (plotSummary 300000.0 1.0 0 0) Nothing)

addLayersFromFile :: FilePath -> Scope -> Scope
addLayersFromFile path scope = scope { layers = layers' }
    where
        layers' = layers scope ++ layersFromFile path

----------------------------------------------------------------

