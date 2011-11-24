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

----------------------------------------------------------------------

data Scope = Scope
    { view :: View
    }

data View = View
    { canvas :: G.DrawingArea
    , adj    :: G.Adjustment
    , viewX  :: Double
    , viewY  :: Double
    , viewW  :: Double
    , viewH  :: Double
    }

scopeNew :: G.DrawingArea -> G.Adjustment -> Scope
scopeNew c adj = Scope {
      view = viewInit c adj
    }

viewInit :: G.DrawingArea -> G.Adjustment -> View
viewInit c adj = View c adj 0.0 (-1.0) 1.0 2.0

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

  adj <- G.adjustmentNew (-1.0) (-1.0) (1.0+0.1) (0.1) 1.0 (0.1)
  drawingArea <- G.drawingAreaNew

  scopeRef <- newIORef (scopeNew drawingArea adj)

  adj `G.onValueChanged` (scroll scopeRef)

  G.boxPackStart vbox drawingArea G.PackGrow 0

  drawingArea `G.on` G.buttonPressEvent $ G.tryEvent $ do
      liftIO $ putStrLn "Button pressed"
  drawingArea `G.on` G.buttonReleaseEvent $ G.tryEvent $ do
      liftIO $ putStrLn "Button released"
  drawingArea `G.on` G.scrollEvent $ G.tryEvent $ wheel scopeRef
  drawingArea `G.on` G.motionNotifyEvent $ G.tryEvent $ motion
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

motion :: G.EventM G.EMotion ()
motion = do
    (x, y) <- G.eventCoordinates
    liftIO $ putStrLn $ printf "motion (%f, %f)" x y

wheel :: IORef Scope -> G.EventM G.EScroll ()
wheel ref = do
    scope <- liftIO $ readIORef ref
    dir <- G.eventScrollDirection
    let mult = case dir of
                   G.ScrollUp   -> 1.1
                   G.ScrollDown -> 0.9
                   _            -> 1.0
        v = view scope
        view' = v { viewW = viewW v * mult }
        scope' = scope { view = view'}
    liftIO $ writeIORef ref scope'
    liftIO $ G.widgetQueueDraw (canvas v)

scroll :: IORef Scope -> IO ()
scroll ref = do
    scope <- readIORef ref
    val <- G.adjustmentGetValue (adj . view $ scope)
    putStrLn $ printf "%f" val

    let v = view scope
        view' = v { viewX = (-1.0) - val }
        scope' = scope { view = view'}
    liftIO $ writeIORef ref scope'
    liftIO $ G.widgetQueueDraw (canvas v)

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
  let width   = 2.0
      height  = 2.0
      xmax    = 1.0
      xmin    = -1.0
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
  -- center origin
  C.translate (width / 2) (height / 2)
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
        dYRange = 200000000.0

        -- Texture
        textureSize = (2^8)+1
        texW = (viewW v) / textureSize
        texH = (viewH v) / textureSize

        t :: I.Iteratee [(TimeStamp, TextureSlice)] C.Render Double
        t = I.foldM renderTex (viewX v)

        renderTex :: Double -> (TimeStamp, TextureSlice) -> C.Render Double
        renderTex x (_ts, (TextureSlice tex)) = mapM_ (uncurry (texVal x)) (zip (iterate (+texH) (viewX v)) tex) >> return (x+texW)

        texVal :: Double -> Double -> Float -> C.Render ()
        texVal x y v = do
            -- liftIO . putStrLn $ printf "(%f, %f) : %f" x y v
            C.setSourceRGB s s (s-0.1)
            C.rectangle x y (texW+0.01) (texH+0.01)
            C.fill
            where
                s = (realToFrac v / 4) + 0.5

        -- raw data
        dSize = 5000
        dW = (viewW v) / fromIntegral dSize

        i :: Double -> Double -> Double -> Double -> I.Iteratee [(TimeStamp, Double)] C.Render ()
        i yR r g b = do
            lift $ C.setSourceRGB r g b
            lift $ m (viewX v) 0
            I.foldM (renderRaw yR) (viewX v)
            lift $ C.stroke

        renderRaw :: Double -> Double -> (TimeStamp, Double) -> C.Render Double
        renderRaw yR x (_ts, y) = do
            -- liftIO . putStrLn $ printf "(%f, %f)" x y
            l x (y * (viewH v)/ yR)
            return (x+dW)

        -- Summary
        sSize = 20
        sW = (viewW v) / fromIntegral sSize

        j :: I.Iteratee [Summary Double] C.Render ()
        j = do
            lift $ C.setSourceRGB 1.0 0 0
            lift $ m (viewX v) 0
            I.foldM renderSummary (viewX v)
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
