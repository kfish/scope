{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module GUI (
    guiMain
) where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.ZoomCache (TimeStamp(..), prettyTimeStamp)
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as M

import Paths_scope as My
import Scope.Layer
import Scope.Types
import Scope.View

import Scope.Cairo

----------------------------------------------------------------------

windowWidth, windowHeight :: Int
windowWidth   = 500
windowHeight  = 500

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
  demoPath <- My.getDataFileName "demo"
  G.fileChooserSetCurrentFolder openDialog demoPath

  opena `G.on` G.actionActivated $ G.widgetShow openDialog

  saveDialog <- fChooser G.FileChooserActionSave G.stockSave
  savea `G.on` G.actionActivated $ G.widgetShow saveDialog
  saveasa `G.on` G.actionActivated $ G.widgetShow saveDialog

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

  let scope = scopeCairoNew drawingArea adj
  scopeRef <- newIORef scope

  mapM_ (modifyIORefM scopeRef . addLayersFromFile) args
  openDialog `G.on` G.response $ myFileOpen scopeRef openDialog
  saveDialog `G.on` G.response $ myFileSave scopeRef saveDialog

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
    , G.PointerMotionMask
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

myFileOpen :: IORef (Scope ViewCairo) -> G.FileChooserDialog -> G.ResponseId -> IO ()
myFileOpen scopeRef fcdialog response = do
  case response of
    G.ResponseAccept -> do
        Just filename <- G.fileChooserGetFilename fcdialog
        scopeModifyMUpdate scopeRef (addLayersFromFile filename)
    _ -> return ()
  G.widgetHide fcdialog

myFileSave :: IORef (Scope ViewCairo) -> G.FileChooserDialog -> G.ResponseId -> IO ()
myFileSave scopeRef fcdialog response = do
  case response of
    G.ResponseAccept -> do
        Just filename <- G.fileChooserGetFilename fcdialog
        writePng filename scopeRef
    _ -> return ()
  G.widgetHide fcdialog

myCut :: IO ()
myCut = putStrLn "Cut"

myCopy :: IO ()
myCopy = putStrLn "Copy"

myPaste :: IO ()
myPaste = putStrLn "Paste"

myDelete :: IO ()
myDelete = putStrLn "Delete"

updateCanvas :: IORef (Scope ViewCairo) -> IO Bool
updateCanvas ref = do
    scope <- readIORef ref
    let c = canvas . viewUI . view $ scope
    win <- G.widgetGetDrawWindow c
    (width, height) <- G.widgetGetSize c
    G.renderWithDrawable win $ plotWindow width height scope
    return True

writePng :: FilePath -> IORef (Scope ViewCairo) -> IO ()
writePng path ref = do
    scope <- readIORef ref
    let c = canvas . viewUI . view $ scope
    (width, height) <- G.widgetGetSize c
    C.withImageSurface C.FormatARGB32 width height $ \ result -> do
        C.renderWith result $ plotWindow width height scope
        C.surfaceWriteToPNG result path

----------------------------------------------------------------

scopeModifyMUpdate :: IORef (Scope ViewCairo)
                   -> (Scope ViewCairo -> IO (Scope ViewCairo))
                   -> IO ()
scopeModifyMUpdate ref f = do
    modifyIORefM ref f
    viewCairoUpdate =<< view <$> readIORef ref

scopeModifyUpdate :: IORef (Scope ViewCairo)
                  -> (View ViewCairo -> View ViewCairo)
                  -> IO ()
scopeModifyUpdate ref f = do
    modifyIORef ref (scopeModifyView f)
    viewCairoUpdate =<< view <$> readIORef ref

viewCairoUpdate :: View ViewCairo -> IO ()
viewCairoUpdate View{..} = do
    G.adjustmentSetValue (adj viewUI) (toDouble viewX1)
    G.adjustmentSetPageSize (adj viewUI) $ toDouble (distance viewX1 viewX2)
    G.widgetQueueDraw (canvas viewUI)

----------------------------------------------------------------

_canvasToScreen :: G.DrawingArea -> CanvasX -> IO ScreenX
_canvasToScreen c (CanvasX cX) = do
    (width, _height) <- G.widgetGetSize c
    return $ ScreenX (fromIntegral width * cX)

screenToCanvas :: ViewCairo -> ScreenX -> IO CanvasX
screenToCanvas vc (ScreenX sX) = do
    (width, _height) <- G.widgetGetSize (canvas vc)
    return $ CanvasX (sX / fromIntegral width)

----------------------------------------------------------------

buttonDown :: IORef (Scope ViewCairo) -> G.EventM G.EButton ()
buttonDown ref = do
    (x, _y) <- G.eventCoordinates
    liftIO $ do
        vc <- viewUI . view <$> readIORef ref
        cX <- screenToCanvas vc (ScreenX x)
        modifyIORef ref (scopeModifyView (viewButtonDown cX))

buttonRelease :: IORef (Scope ViewCairo) -> G.EventM G.EButton ()
buttonRelease ref = liftIO $ modifyIORef ref (scopeModifyView viewButtonRelease)

motion :: IORef (Scope ViewCairo) -> G.EventM G.EMotion ()
motion ref = do
    (x, _y) <- G.eventCoordinates
    liftIO $ do
        View{..} <- view <$> readIORef ref
        cX <- screenToCanvas viewUI (ScreenX x)
        scopeModifyUpdate ref (viewButtonMotion cX)

wheel :: IORef (Scope ViewCairo) -> G.EventM G.EScroll ()
wheel ref = do
    (x, _y) <- G.eventCoordinates
    dir <- G.eventScrollDirection
    liftIO $ do
        scope <- readIORef ref
        let View{..} = view scope
        cX <- screenToCanvas viewUI (ScreenX x)
        case dir of
            G.ScrollUp   -> scopeModifyUpdate ref (viewZoomInOn cX 1.2)
            G.ScrollDown -> scopeModifyUpdate ref (viewZoomOutOn cX 1.2)
            _            -> return ()

scroll :: IORef (Scope ViewCairo) -> IO ()
scroll ref = do
    val <- G.adjustmentGetValue =<< adj . viewUI . view <$> readIORef ref
    scopeModifyUpdate ref (viewMoveTo val)

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

keyDown :: IORef (Scope ViewCairo) -> G.EventM G.EKey ()
keyDown ref = do
    v <- G.eventKeyVal
    -- n <- G.eventKeyName
    -- liftIO . putStrLn $ printf "Key %s (%d) pressed" n v
    let f = case v of
                XK_Home -> Just viewMoveStart
                XK_End  -> Just viewMoveEnd
                XK_Up   -> Just $ viewZoomIn 2.0
                XK_Down -> Just $ viewZoomOut 2.0
                XK_Left  -> Just viewMoveRight
                XK_Right -> Just viewMoveLeft
                _ -> Nothing

    maybe (return ()) (liftIO . scopeModifyUpdate ref) f

----------------------------------------------------------------

plotWindow :: Int -> Int -> Scope ViewCairo -> C.Render ()
plotWindow width height scope = do
    prologue width height
    plotLayers scope
    plotTimeline scope
    plotCursor scope

-- Set up stuff
prologue :: Int -> Int -> C.Render ()
prologue wWidth wHeight = do
  -- Define viewport coords as (-1.0, -1.0) - (1.0, 1.0)
  let width   = 1.0
      height  = 2.0
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

----------------------------------------------------------------------

plotCursor :: Scope ViewCairo -> C.Render ()
plotCursor scope = maybe (return ()) f pointerX
    where
        View{..} = view scope
        f :: CanvasX -> C.Render ()
        f (CanvasX cX) = do
            C.setSourceRGBA 0 0.7 0 0.4
            C.moveTo cX (-1.0)
            C.lineTo cX 1.0
            C.stroke

----------------------------------------------------------------------

plotTimeline :: Scope ViewCairo -> C.Render ()
plotTimeline scope = do
    case (dataToTimeStamp scope viewX1, dataToTimeStamp scope viewX2) of
        (Just s, Just e) -> do
            plotAllTicks s e
            plotAllLabels s e
        _                -> return ()
    maybe (return ()) plotArrow pointerX
    where
        View{..} = view scope

        plotAllTicks :: TimeStamp -> TimeStamp -> C.Render ()
        plotAllTicks s e = do
            plotTicks 0.001 0.05 s e
            plotTicks 0.01 0.1 s e
            plotTicks 0.02 1.0 s e
            plotTicks 0.04 5.0 s e
            plotTicks 0.06 10.0 s e
            plotTicks 0.08 60.0 s e
            plotTicks 0.10 3600.0 s e

        plotTicks :: Double -> Double -> TimeStamp -> TimeStamp -> C.Render ()
        plotTicks len step (TS start) (TS end) =
            when doDraw $ mapM_ (plotTick len) (map TS [s, s+step .. end])
            where
                doDraw = (end - start) / step < 100
                s = (fromIntegral (floor (start/step) :: Integer)) * step

        plotTick :: Double -> TimeStamp -> C.Render ()
        plotTick len ts = do
            let (CanvasX cX) = timeStampToCanvas scope ts
            C.setSourceRGBA 0 0 0 1.0
            C.moveTo cX 0.90
            C.lineTo cX (0.90 + len)
            C.stroke

        plotAllLabels :: TimeStamp -> TimeStamp -> C.Render ()
        plotAllLabels (TS start) (TS end) =
            mapM_ (\s -> plotLabels s (TS start) (TS end)) steps
            where
                readable x = let viz = (end - start) / x in (viz < 5 && viz >= 1)
                steps = take 1 . filter readable $ [3600, 60, 10, 5, 1, 0.1, 0.05]

        plotLabels :: Double -> TimeStamp -> TimeStamp -> C.Render ()
        plotLabels step (TS start) (TS end) = keepState $ do
            let flipY = M.Matrix 1 0 0 (-2.2) 0 0
            C.transform flipY

            let s = (fromIntegral (floor (start/step) :: Integer)) * step
            mapM_ (plotLabel . TS) [s, s+step .. end]

        plotLabel :: TimeStamp -> C.Render ()
        plotLabel ts = do
            let CanvasX cX = timeStampToCanvas scope ts
            drawString (prettyTimeStamp ts) cX (-0.44)

drawString :: String -> Double -> Double -> C.Render ()
drawString s x y = do
    C.selectFontFace "sans" C.FontSlantNormal C.FontWeightNormal
    C.setFontSize 0.02
    C.moveTo x y
    C.textPath s
    C.fillPreserve

plotArrow :: CanvasX -> C.Render ()
plotArrow (CanvasX cX) = do
    C.setSourceRGBA 0 0 0 0.9
    C.moveTo (cX-0.004) (0.99)
    C.lineTo (cX+0.004) (0.99)
    C.lineTo cX (0.98)
    C.fill

----------------------------------------------------------------------

modifyIORefM :: IORef a -> (a -> IO a) -> IO ()
modifyIORefM ref f = do
    x <- readIORef ref
    x' <- f x
    writeIORef ref x'

