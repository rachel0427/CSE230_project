-- Main.hs
-- testing brick
{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Brick
import Brick.BChan
import Graphics.Vty.Attributes (defAttr)

app :: App () e ()
app = App
  { appDraw = const [str "Hello, Brick!"]
  , appChooseCursor = neverShowCursor
  , appHandleEvent = \case
      -- Handle any key press to exit
      _ -> halt
  , appStartEvent = pure ()
  , appAttrMap = const $ attrMap defAttr []
  }

main :: IO ()
main = defaultMain app ()
