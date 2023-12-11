-- UI.hs
module UI where

import Brick
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (border, borderWithLabel, borderAttr)
import Brick.Widgets.Border.Style (unicode)
import Control.Monad (void)
import Graphics.Vty
import Graphics.Vty.Input.Events (Key (KChar), Event (EvKey))
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)

import Types
import Game
import Activity

data UIState = Menu | StartGame PlayStatus deriving (Show, Eq)

data CustomEvent = StartNewGame | ExitGame deriving (Show, Eq)

type Name = ()

app :: App UIState CustomEvent Name
app = App
    { appDraw         = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return -- do nothing
    , appAttrMap = const $ attrMap (bg oliveGreen) 
                    [ (attrBlue, fg cerulean), (attrGreen, fg yellowGreen), (attrWhite, fg offWhite), (attrRed, fg red)
                    , (attrLilac, fg lilac)]
    -- const $ attrMap Graphics.Vty.defAttr
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
    withAttr attrWhite $
    borderWithLabel (str "Main Menu") $
    hCenter $
    vBox
        [ withAttr attrGreen $ str "Press 's' to start a new game."
        , withAttr attrRed $ str "Press 'q' to exit the game."
        ]

-- Function to read ASCII art from a file
readArtFromFile :: FilePath -> IO String
readArtFromFile filePath = readFile filePath

getArtResource :: Weather -> String
getArtResource weather | weather == Sunny = sunny fixArt
                       | weather == Rainy = rainy fixArt
                       | otherwise = cloudy fixArt
                      --  | weather == Cloudy = rainy fixArt

-- getArtResource :: Weather -> Widget Name
-- getArtResource weather | weather == Sunny = sunnyW fixArtWidget
--                        | weather == Rainy = rainyW fixArtWidget
--                        | otherwise = cloudyW fixArtWidget

uiStartGame :: UIState -> Widget Name 
uiStartGame (StartGame st) = 
    -- let weatherWidget = getArtResource (weather st) -- Replace this with your character representation
    let textArtResource = getArtResource (weather st) -- Replace this with your character representation
        weatherWidget = withAttr attrBlue $ withBorderStyle unicode $ strWrap textArtResource
        
    in
    -- let textArtResource = " (*.*)/  \n" ++ " <)  )  \n" ++ "  /  \\  \n" -- Replace this with your character representation
    --     weatherWidget = withBorderStyle unicode $ strWrap textArtResource
        
    -- in
    center $ vLimit 100 $ hLimit 100 $
    withAttr attrWhite $
    borderWithLabel (str $ "Days survived: " ++ show (date st)) $
    hCenter $
    vBox
        [ hBox -- Use hBox to horizontally concatenate widgets
            [ vBox [str (" Weather: " ++ show (weather st))
              , hBox
                [weatherWidget, vBox [center $ vLimit 20 $ withAttr attrGreen $ borderWithLabel (str "Character Status") $ padTop (Pad 1) $ hBox [vBox
                      [ hCenter $ str $ "Health: " ++ show (health st)
                      , hCenter $ str $ "Hunger: " ++ show (hunger st)
                      , hCenter $ str $ "Thirsty: " ++ show (thirsty st)
                      , hCenter $ str $ " "
                      ], withBorderStyle unicode $ strWrap textArtResource], 
                      center $ vLimit 20 $ withAttr attrBlue $ borderWithLabel (str "Previous action:") $ padTop (Pad 1) $ vBox
                      [ strWrap $ activityText (prevActivity st)
                      ] 
                    ]  
                ]
              ]
            ]
        , center $ vLimit 20 $ withAttr attrLilac $ borderWithLabel (str "Select an action:") $ padTop (Pad 1) $ vBox
            [ str $ "w. " ++ (getDescription st 'W')
            , str $ "a. " ++ (getDescription st 'A')
            , str $ "s. " ++ (getDescription st 'S')
            , str $ "d. " ++ (getDescription st 'D')
            , hCenter $ str $ " "
            ]
        , withAttr attrRed $ str "Press 'q' to return to main menu."
        ]
      


handleEvent :: UIState -> BrickEvent Name CustomEvent -> EventM Name (Next UIState)
handleEvent Menu (VtyEvent (EvKey (KChar 's') [])) = liftIO initNewGame >>= continue
handleEvent Menu (VtyEvent (EvKey (KChar 'q') [])) = halt Menu
-- handleEvent (StartGame ps@(PlayStatus hunger thirsty health weather date alive aM)) (VtyEvent (EvKey (KChar 'a') [])) =
--   continue $ StartGame $ PlayStatus (max 0 (hunger - 10)) (max 0 (thirsty - 10)) health weather (date+1) alive aM

handleEvent (StartGame ps@(PlayStatus hunger thirsty health weather date alive aM prev)) (VtyEvent (EvKey (KChar 'w') [])) = do
  newPS <- liftIO $ updatePlayStatusWithChar 'W' ps
  continue $ StartGame newPS
  
handleEvent (StartGame ps@(PlayStatus hunger thirsty health weather date alive aM prev)) (VtyEvent (EvKey (KChar 'a') [])) = do
  newPS <- liftIO $ updatePlayStatusWithChar 'A' ps
  continue $ StartGame newPS

handleEvent (StartGame ps@(PlayStatus hunger thirsty health weather date alive aM prev)) (VtyEvent (EvKey (KChar 's') [])) = do
  newPS <- liftIO $ updatePlayStatusWithChar 'S' ps
  continue $ StartGame newPS

handleEvent (StartGame ps@(PlayStatus hunger thirsty health weather date alive aM prev)) (VtyEvent (EvKey (KChar 'd') [])) = do
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
                                activityMap = randomMap,
                                prevActivity = NoActivity }