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

data Scope = Scope
    { view :: View
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
      C.renderWith result $ example width height
      C.surfaceWriteToPNG result "Draw.png"
  where width  = windowWidth
        height = windowHeight
-}

-- Display image in window
guiMain :: Chan String -> IO ()
guiMain chan = do
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
    G.renderWithDrawable win $ example width height scope
    return True

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
                       G.ScrollDown -> 1.1
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

drawCircle :: Double -> Double -> Double -> C.Render ()
drawCircle x y r = do
  C.arc x y r 0 (2 * pi)
  fillStroke

drawRectangle :: Double -> Double -> Double -> Double -> C.Render ()
drawRectangle x y w h = do
  C.rectangle x y w h
  fillStroke

stroke :: C.Render ()
stroke =
  keepState $ do
  C.setSourceRGBA 0 0 0 0.7
  C.stroke

fillStroke :: C.Render ()
fillStroke = do
  C.fillPreserve
  stroke

----------------------------------------------------------------

-- Example

example :: Int -> Int -> Scope -> C.Render ()
example width height scope = do
    prologue width height (view scope)
    plot1 scope

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

instance MonadCatchIO C.Render where
  m `catch` f = mapRender (\m' -> m' `catch` \e -> runRender $ f e) m
  block       = mapRender block
  unblock     = mapRender unblock

mapRender f = Render . f . runRender

plot1 :: Scope -> C.Render ()
plot1 scope = keepState $ do
    -- let dataPath = "../zoom-cache/foo.zoom"
    let dataPath = "/home/conrad/src/tsuru/trader/272-log/spot+pl.zoom"
        texturePath = "../texture-synthesis/texture.zoom"

    -- Render texture
    I.fileDriverRandom (I.joinI $ enumCacheFile textureIdentifiers (I.joinI $ enumTexture t)) texturePath

    -- Render raw data
    I.fileDriverRandom (I.joinI $ enumCacheFile standardIdentifiers (I.joinI . filterTracks [1] . I.joinI . enumDouble . I.joinI . I.take dSize $ i dYRange 0.3 0.7 0.2)) dataPath
    I.fileDriverRandom (I.joinI $ enumCacheFile standardIdentifiers (I.joinI . filterTracks [2] . I.joinI . enumDouble . I.joinI . I.take dSize $ i 30000 0.7 0.3 0.2)) dataPath

    -- Render summary data
    I.fileDriverRandom (I.joinI $ enumCacheFile standardIdentifiers (I.joinI . filterTracks [1] $ enumSummaryDouble 2 . I.joinI . I.take sSize $ j)) dataPath

    where
        m = C.moveTo
        l = C.lineTo

        v = view scope

        -- dYRange = 1000.0
        -- dYRange = 200000000.0
        dYRange = 1000000000.0

        canvasFold f = I.foldM f (negate . toDouble $ (viewX1 v))

        stepWidth s = 1.0 / (toDouble (distance (viewX1 v) (viewX2 v)) * fromIntegral s)

        -- Texture
        textureSize = (2^5)+1
        texW = stepWidth textureSize
        texH = (viewY2 v - viewY1 v) / fromIntegral textureSize

        t :: I.Iteratee [(TimeStamp, TextureSlice)] C.Render Double
        t = canvasFold renderTex

        renderTex :: Double -> (TimeStamp, TextureSlice) -> C.Render Double
        renderTex x (_ts, (TextureSlice tex)) = do
            mapM_ (uncurry (texVal x)) (zip (iterate (+texH) (viewY1 v)) tex)
            return (x+texW)

        texVal :: Double -> Double -> Float -> C.Render ()
        texVal x y v = do
            -- liftIO . putStrLn $ printf "(%f, %f) : %f" x y v
            C.setSourceRGB s (s*0.9) (s-0.03)
            C.rectangle x y (texW+0.01) (texH+0.01)
            C.fill
            where
                s = (realToFrac v / 4) + 0.75

        -- raw data
        dSize = 5000
        dW = stepWidth dSize

        i :: Double -> Double -> Double -> Double -> I.Iteratee [(TimeStamp, Double)] C.Render ()
        i yR r g b = do
            lift $ C.setSourceRGB r g b
            canvasFold (renderRaw yR)
            lift $ C.stroke

        renderRaw :: Double -> Double -> (TimeStamp, Double) -> C.Render Double
        renderRaw yR x (_ts, y) = do
            -- liftIO . putStrLn $ printf "(%f, %f)" x y
            l x (y * (viewY2 v - viewY1 v)/ yR)
            return (x+dW)

        -- Summary
        sSize = 20
        sW = stepWidth sSize

        j :: I.Iteratee [Summary Double] C.Render ()
        j = do
            lift $ C.setSourceRGB 1.0 0 0
            canvasFold renderSummary
            lift $ C.stroke

        renderSummary :: Double -> Summary Double -> C.Render Double
        renderSummary x s = do
            -- liftIO . putStrLn $ printf "(%f, %f)" x y
            l x y
            return (x+sW)
            where
                y = fx s * 4.0 / dYRange

        fx :: Summary Double -> Double
        fx = numMax . summaryData

-- doubles :: [Double]
-- doubles = take 100 $ map ((* 5.0) . sin) [0.0, 0.1 ..]

example1 :: C.Render ()
example1 = do
  -- circles
  drawCircle 0 0 1
  drawCircle 2 2 3
  -- a bunch of rectangles
  keepState $
    replicateM_ 5 $ do
        drawRectangle 0 1 2 3
        C.rotate (pi/8)
  -- some cute stuff
  thought
  apple
  snake

thought :: C.Render ()
thought =
  keepState $ do
  C.scale 0.04 0.04
  C.translate (200) (380)
  C.rotate pi
  C.setSourceRGBA 0.5 0.5 1 0.7
  C.setLineWidth 1
  image
  fillStroke
  where
    m = C.moveTo
    c = C.curveTo
    z = C.closePath
    image = do
        m 184 327
        c 176 327 170 332 168 339
        c 166 333 160 329 153 329
        c 147 329 141 333 138 339
        c 137 339 136 338 134 338
        c 125 338 118 345 118 354
        c 118 363 125 371 134 371
        c 137 371 140 370 142 368
        c 142 368 142 368 142 369
        c 142 377 149 385 158 385
        c 162 385 166 383 168 381
        c 171 386 176 390 183 390
        c 188 390 193 387 196 383
        c 198 384 201 385 204 385
        c 212 385 220 378 220 369
        c 222 371 225 372 228 372
        c 237 372 244 364 244 355
        c 244 346 237 339 228 339
        c 227 339 226 339 225 340
        c 223 332 217 327 209 327
        c 204 327 199 330 196 333
        c 193 330 189 327 184 327
        z
        m 164 387
        c 158 387 153 391 153 397
        c 153 402 158 407 164 407
        c 170 407 174 402 174 397
        c 174 391 170 387 164 387
        z
        m 152 408
        c 149 408 146 411 146 414
        c 146 417 149 420 152 420
        c 155 420 158 417 158 414
        c 158 411 155 408 152 408
        z
        m 143 422
        c 141 422 139 424 139 426
        c 139 428 141 429 143 429
        c 144 429 146 428 146 426
        c 146 424 144 422 143 422
        z

apple :: C.Render ()
apple =
  keepState $ do
  C.scale 0.05 0.05
  C.translate (1110) (220)
  C.rotate pi
  C.setLineWidth 0.5
  C.setSourceRGBA 0 0 0 0.7
  image1
  fillStroke
  C.setSourceRGBA 1 0 0 0.7
  image2
  fillStroke
  where
    m = C.moveTo
    c = C.curveTo
    z = C.closePath
    l = C.lineTo
    image1 = do
        m 1149 245
        l 1156 244
        l 1155 252
        l 1149 245
        z
    image2 = do
        m 1151 249
        c 1145 249 1140 254 1140 261
        c 1140 268 1145 273 1151 273
        c 1152 273 1153 273 1154 272
        c 1156 273 1157 273 1158 273
        c 1164 273 1169 268 1169 261
        c 1169 254 1164 249 1158 249
        c 1157 249 1156 249 1154 250
        c 1153 249 1152 249 1151 249
        z

snake :: C.Render ()
snake =
  keepState $ do
  C.scale 0.04 0.04
  C.translate (150) (220)
  C.rotate pi
  C.setLineWidth 0.5
  C.setSourceRGBA 0.1 0.1 0 0.7
  image
  fillStroke
  where
    m = C.moveTo
    c = C.curveTo
    z = C.closePath
    l = C.lineTo
    image = do
        m 146 320
        c 143 308 130 314 123 319
        c 115 324 108 311 100 314
        c  93 317  92 319  81 318
        c  76 318  60 309  60 320
        c  60 328  73 321  82 323
        c  94 326  98 317 106 320
        c 113 323 120 330 128 323
        c 133 318 142 312 146 320
        l 146 320
        z

----------------------------------------------------------------
