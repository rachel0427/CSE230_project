-- Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Vty
import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center, vCenter)
import Brick.Widgets.Core (str)
import Brick.Widgets.Core ( (<+>) )

import qualified UI as UI
import qualified Game as Game

-- | The main function to run the Brick application
main :: IO ()
main = do
    -- Create the Brick app
    _ <- defaultMain UI.app UI.initialUIState