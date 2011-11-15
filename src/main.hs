{-# OPTIONS -Wall #-}

module Main where

import Control.Concurrent.Chan
import Control.Monad (when)
import Data.ZoomCache
import Data.ZoomCache.Dump
import System.Environment (getArgs)

import Children
import GUI

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> return ()
        (path:_) -> zoomDumpFile standardIdentifiers 1 path
    setupMonitor

setupMonitor :: IO ()
setupMonitor = later waitForChildren $ do
    chan <- newChan
    _ <- forkChild (guiMain chan)
    monitor chan
    where
        later x y = y >> x

monitor :: Chan String -> IO()
monitor chan = do
    x <- readChan chan
    putStrLn $ "Received message: " ++ x
    when (x /= "quit") $ monitor chan
