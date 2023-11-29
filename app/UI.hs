-- UI.hs
module UI where

import Brick
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Control.Monad (void)
import Graphics.Vty
import Graphics.Vty.Input.Events (Key (KChar), Event (EvKey))

import Types

data UIState = Menu | StartGame PlayStatus deriving (Show, Eq)

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
initNewGame = StartGame PlayStatus {hunger = 0, thirsty = 0, health = 100, weather = Sunny, date = 1}

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
uiStartGame (StartGame st) = 
    center $ vLimit 100 $ hLimit 100 $
    borderWithLabel (str $ "Days survived: " ++ show (date st)) $
    hCenter $
    vBox
        [ vBox [str (" Weather: " ++ show (weather st))]
          , center $ vLimit 20 $ borderWithLabel (str "Character Status") $ padTop (Pad 1) $ vBox
            [ str $ "Health " ++ show (health st)
            , str $ "Hunger " ++ show (hunger st)
            , str $ "Thirsty " ++ show (thirsty st)
            ]
        , center $ vLimit 20 $ borderWithLabel (str "Select an action:") $ padTop (Pad 1) $ vBox
            [ str $ "a. " ++ (op1 demoOption)
            , str $ "b. " ++ (op2 demoOption)
            , str $ "c. " ++ (op3 demoOption)
            , str $ "d. " ++ (op4 demoOption)
            ]
        ]
      

-- uiStartGame st = center $ vBox
-- uiStartGame st = center $ vBox
--   [ str "You are in the game!"
--   , str $ "Hunger: " ++ show (hunger st)
--   , str $ "Health: " ++ show (health st)
--   , str $ "Weather: " ++ show (weather st)
--   , str $ "Date: " ++ show (date st)
--   ]
-- uiStartGame =
--     center $
--     borderWithLabel (str "New Game Page") $
--     hCenter $
--     vBox [str "Start new game!"]

handleEvent :: UIState -> BrickEvent Name CustomEvent -> EventM Name (Next UIState)
handleEvent Menu (VtyEvent (EvKey (KChar 's') [])) = continue initNewGame
handleEvent Menu (VtyEvent (EvKey (KChar 'q') [])) = halt Menu
handleEvent (StartGame (PlayStatus hunger thirsty health weather date)) (VtyEvent (EvKey (KChar 'a') [])) =
  continue $ StartGame $ PlayStatus (max 0 (hunger - 10)) (max 0 (thirsty - 10)) health weather (date+1)
handleEvent _ _ = continue Menu