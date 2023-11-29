-- UI.hs
module UI where

import Brick
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Control.Monad (void)
import Graphics.Vty
import Graphics.Vty.Input.Events (Key (KChar), Event (EvKey))

-- import Types

data UIState = Menu | StartGame
  { hunger :: Int
  , health :: Int
--   , weather :: Weather
  , weather :: Int
  , date :: Int
  } deriving (Show, Eq)

data CustomEvent = StartNewGame | ExitGame deriving (Show, Eq)

type Name = ()

app :: App UIState CustomEvent Name
app = App
    { appDraw         = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return -- do nothing
    , appAttrMap = const $ attrMap Graphics.Vty.defAttr []
    }

initNewGame :: UIState
initNewGame = StartGame {hunger = 0
                        , health = 100
                        , weather = 0
                        , date = 1}

drawUI :: UIState -> [Widget Name]
drawUI Menu = [ui]
drawUI initNewGame = [uiStartGame initNewGame]

ui :: Widget Name
ui =
    center $
    borderWithLabel (str "Main Menu") $
    hCenter $
    vBox
        [ str "Press 's' to start a new game."
        , str "Press 'q' to exit the game."
        ]

uiStartGame :: UIState -> Widget Name 
-- uiStartGame st = center $ vBox
uiStartGame st = center $ vBox
  [ str "You are in the game!"
  , str $ "Hunger: " ++ show (hunger st)
  , str $ "Health: " ++ show (health st)
  , str $ "Weather: " ++ show (weather st)
  , str $ "Date: " ++ show (date st)
  ]
-- uiStartGame =
--     center $
--     borderWithLabel (str "New Game Page") $
--     hCenter $
--     vBox [str "Start new game!"]

handleEvent :: UIState -> BrickEvent Name CustomEvent -> EventM Name (Next UIState)
handleEvent Menu (VtyEvent (EvKey (KChar 's') [])) = continue initNewGame
handleEvent Menu (VtyEvent (EvKey (KChar 'q') [])) = halt Menu
handleEvent (StartGame hlt hng w dt) (VtyEvent (EvKey (KChar 'a') [])) =
  continue $ StartGame (max 0 (hlt - 10)) (max 0 (hng - 10)) w dt
handleEvent _ _ = continue Menu