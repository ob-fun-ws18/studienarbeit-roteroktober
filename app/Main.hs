{-# LANGUAGE OverloadedStrings #-}
module Main where

  import Server
  import Gui

  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core
  import Control.Concurrent (forkIO)

  main :: IO ()
  main = do
      forkIO server
      startGUI defaultConfig {-{ jsPort = Just 8023, jsAddr = Just "127.0.0.1" }-} setup