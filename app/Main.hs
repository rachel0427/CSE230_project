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

-- Function to map keys to activities
assignActivitiesToKeys :: IO (M.Map Char Activity)
assignActivitiesToKeys = do
  foragingActivity <- getRandomForagingActivity
  huntingActivity <- getRandomHuntingActivity
  restingActivity <- getRandomRestingActivity
  randomActivity <- getRandomRandomActivity

  let activityMap =
        M.fromList
          [ ('W', Foraging foragingActivity),
            ('A', Hunting huntingActivity),
            ('S', Resting restingActivity),
            ('D', Random randomActivity)
          ]
  return activityMap

-- | The main function to run the Brick application
main :: IO ()
main = do
  let initialState = UI.Menu -- Replace with your actual initial state
  endState <- defaultMain UI.app initialState
  putStrLn "Game over!" -- You can replace this with any post-game logic or message
  -- main :: IO ()
  -- -- main = do
  -- --     -- Create the Brick app
  -- --     -- _ <- defaultMain UI.app UI.initialUIState
  -- --     _ <- defaultMain UI.app UI.Menu