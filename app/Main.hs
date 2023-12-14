-- Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Activity
import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center, vCenter)
import Brick.Widgets.Core (str, (<+>))
import Data.Map as M
import Game
import Graphics.Vty
import UI

-- Main.hs

-- | The main function to run the Brick application
main :: IO ()
main = do
  let initialState = UI.Menu
  endState <- defaultMain UI.app initialState
  putStrLn "Game over!"