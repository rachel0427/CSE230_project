-- UI.hs
module UI where

import Brick
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Control.Monad (void)
import Graphics.Vty
import Graphics.Vty.Input.Events (Key (KChar), Event (EvKey))

data UIState = Menu | StartGame deriving (Show, Eq)

data CustomEvent = StartNewGame | ExitGame deriving (Show, Eq)

type Name = ()

app :: App UIState CustomEvent Name
app = App
    { appDraw         = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return
    -- , appAttrMap      = const $ attrMap defAttr []
    , appAttrMap = const $ attrMap Graphics.Vty.defAttr []
    }

drawUI :: UIState -> [Widget Name]
drawUI Menu = [ui]
drawUI StartGame = [uiStartGame]

ui :: Widget Name
ui =
    center $
    borderWithLabel (str "Main Menu") $
    hCenter $
    vBox
        [ str "Press 's' to start a new game."
        , str "Press 'q' to exit the game."
        ]

uiStartGame :: Widget Name
uiStartGame =
    center $
    borderWithLabel (str "New Game Page") $
    hCenter $
    vBox [str "Start new game!"]

handleEvent :: UIState -> BrickEvent Name CustomEvent -> EventM Name (Next UIState)
handleEvent Menu (VtyEvent (EvKey (KChar 's') [])) = continue StartGame
handleEvent Menu (VtyEvent (EvKey (KChar 'q') [])) = halt Menu
handleEvent _ _ = continue Menu
