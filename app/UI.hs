-- UI.hs
module UI where

import Brick
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Control.Monad (void)
import Graphics.Vty
import Graphics.Vty.Input.Events (Key (KChar), Event (EvKey))
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)

import Types
import Game

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

initNewGame :: IO UIState
-- initNewGame = StartGame PlayStatus {hunger = 0, thirsty = 0, health = 100, weather = Sunny, date = 1}
initNewGame = randomInitNewGame

drawUI :: UIState -> [Widget Name]
drawUI Menu = [ui]
-- drawUI initNewGame = [uiStartGame initNewGame]
drawUI st = [uiStartGame st]

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
    let textArtResource = ""
    in
    let image = string defAttr textArtResource
    in
    center $ vLimit 100 $ hLimit 100 $
    borderWithLabel (str $ "Days survived: " ++ show (date st)) $
    hCenter $
    vBox
        [ vBox [str (" Weather: " ++ show (weather st)), raw image]
          , center $ vLimit 20 $ borderWithLabel (str "Character Status") $ padTop (Pad 1) $ vBox
            [ hCenter $ str $ "Health " ++ show (health st)
            , hCenter $ str $ "Hunger " ++ show (hunger st)
            , hCenter $ str $ "Thirsty " ++ show (thirsty st)
            , hCenter $ str $ " "
            ]
        , center $ vLimit 20 $ borderWithLabel (str "Select an action:") $ padTop (Pad 1) $ vBox
            [ str $ "a. " ++ (op1 demoOption)
            , str $ "b. " ++ (op2 demoOption)
            , str $ "c. " ++ (op3 demoOption)
            , str $ "d. " ++ (op4 demoOption)
            , hCenter $ str $ " "
            ]
        ]
      


handleEvent :: UIState -> BrickEvent Name CustomEvent -> EventM Name (Next UIState)
handleEvent Menu (VtyEvent (EvKey (KChar 's') [])) = liftIO initNewGame >>= continue
handleEvent Menu (VtyEvent (EvKey (KChar 'q') [])) = halt Menu
-- handleEvent (StartGame ps@(PlayStatus hunger thirsty health weather date alive aM)) (VtyEvent (EvKey (KChar 'a') [])) =
--   continue $ StartGame $ PlayStatus (max 0 (hunger - 10)) (max 0 (thirsty - 10)) health weather (date+1) alive aM

handleEvent (StartGame ps@(PlayStatus hunger thirsty health weather date alive aM)) (VtyEvent (EvKey (KChar 'w') [])) = do
  newPS <- liftIO $ updatePlayStatusWithChar 'W' ps
  continue $ StartGame newPS
  
handleEvent (StartGame ps@(PlayStatus hunger thirsty health weather date alive aM)) (VtyEvent (EvKey (KChar 'a') [])) = do
  newPS <- liftIO $ updatePlayStatusWithChar 'A' ps
  continue $ StartGame newPS

handleEvent (StartGame ps@(PlayStatus hunger thirsty health weather date alive aM)) (VtyEvent (EvKey (KChar 's') [])) = do
  newPS <- liftIO $ updatePlayStatusWithChar 'S' ps
  continue $ StartGame newPS

handleEvent (StartGame ps@(PlayStatus hunger thirsty health weather date alive aM)) (VtyEvent (EvKey (KChar 'd') [])) = do
  newPS <- liftIO $ updatePlayStatusWithChar 'D' ps
  continue $ StartGame newPS

handleEvent _ _ = continue Menu

-- random generate initial state
randomInitNewGame :: IO UIState
randomInitNewGame = do
--   let initialHealth = 100
--   let initialDate = 1
--   let initialHunger = 0
--   let initialThirst = 0
  randomWeather <- getRandomWeather
  randomMap <- assignActivitiesToKeys
  return $ StartGame PlayStatus { hunger = 100, 
                                thirsty = 100, 
                                health = 100, 
                                weather = randomWeather, 
                                date = 1,
                                alive = True,
                                activityMap = randomMap }