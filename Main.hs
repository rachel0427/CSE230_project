-- Main.hs
-- testing brick
module Main where

import Brick

app :: App () e ()
app = App
  { appDraw = const [str "Hello, Brick!"]
  , appChooseCursor = neverShowCursor
  , appHandleEvent = const id
  , appStartEvent = return
  , appAttrMap = const $ attrMap defAttr []
  }

main :: IO ()
main = defaultMain app ()
