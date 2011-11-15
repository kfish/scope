{-# OPTIONS -Wall #-}

module Main where

import Control.Concurrent.Chan
import Control.Monad (when)

import Children
import GUI

main :: IO ()
main = do
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
