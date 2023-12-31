-- UI.hs
module UI where

import Brick
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (border, borderWithLabel, borderAttr)
import Brick.Widgets.Border.Style (unicode, unicodeBold)
import qualified Brick.Widgets.Core as Core
import Control.Monad (void, when)
import Graphics.Vty
import Graphics.Vty.Input.Events (Key (KChar), Event (EvKey))
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

import Types
import Game
import Activity

data UIState = Menu | StartGame PlayStatus | EndGame Int deriving (Show, Eq)

data CustomEvent = StartNewGame | ExitGame deriving (Show, Eq)

data MenuItem = NewGame | Exit deriving (Show, Eq)

type Name = ()

dateLimit :: Int
dateLimit = 20

app :: App UIState CustomEvent Name
app = App
    { appDraw         = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return -- do nothing
    , appAttrMap = const $ attrMap (bg oliveGreen) 
                    [ (attrBlue, fg cerulean), (attrGreen, fg yellowGreen), (attrWhite, fg offWhite), (attrRed, fg red)
                    , (attrLilac, fg lilac), (attrBold, withStyle (fg lilac) bold), (attrBoldW, withStyle (fg offWhite) bold) ]
    }

initNewGame :: IO UIState
initNewGame = randomInitNewGame

drawUI :: UIState -> [Widget Name]
drawUI Menu = [ui]
drawUI (EndGame res) = [ui2 res]
drawUI (StartGame ps) = [uiStartGame (StartGame ps)]

ui :: Widget Name
ui =
    center $
    withAttr attrWhite $
    withBorderStyle unicodeBold $
    borderWithLabel (str " Main Menu ") $
    hCenter $
    vBox
        [ withAttr attrGreen $ str "Press 's' to start a new game."
        , withAttr attrRed $ str "Press 'q' to exit the game."
        ]

ui2 :: Int -> Widget Name
ui2 res = 
      center $
      withBorderStyle unicodeBold $
      withAttr attrWhite $
      borderWithLabel (str " Game Over ") $
      hCenter $
      vBox
          [ if res == 1 then (withAttr attrGreen $ str "You win!") else (withAttr attrRed $ str "You lose!")
          , withAttr attrBlue $ str "Press 'q' to return to the menu."
          ]



getArtResource :: Weather -> Widget Name
getArtResource weather | weather == Sunny = sunnyW fixArtWidget
                       | weather == Rainy = rainyW fixArtWidget
                       | otherwise = cloudyW fixArtWidget

uiStartGame :: UIState -> Widget Name 
uiStartGame (StartGame st) = 
    let weatherWidget = getArtResource (weather st)
        characterResource = " (*.*)/  \n" ++ " <)  )  \n" ++ "  /  \\  \n"       
    in
    center $ vLimit 150 $ hLimit 200 $
    withAttr attrWhite $
    withBorderStyle unicodeBold $
    borderWithLabel (str $ " Days survived: " ++ show (date st) ++ " ") $
    hCenter $
    vBox
        [ hBox -- Use hBox to horizontally concatenate widgets
            [ vBox [str (" Weather: " ++ show (weather st))
              , hBox
                [str "  ", 
                vBox [weatherWidget, islandWidget], 
                vBox [center $ vLimit 20 $ withAttr attrGreen $ borderWithLabel (str " Character Status ") $ padTop (Pad 1) $ hBox [vBox
                      [ hCenter $ str $ "Health: " ++ show (health st)
                      , hCenter $ str $ "Hunger: " ++ show (hunger st)
                      , hCenter $ str $ "Thirsty: " ++ show (thirsty st)
                      , hCenter $ str $ " "
                      ], withBorderStyle unicode $ withAttr attrLilac $ strWrap characterResource], 
                      center $ vLimit 20 $ withAttr attrBlue $ borderWithLabel (str " Previous action ") $ padTop (Pad 1) $ vBox
                      [
                        vBox [strWrap $ activityText (prevActivity st), hCenter $ str $ " "] 
                      ] 
                    ]  
                ]
              ]
            ]
        , center $ vLimit 20 $ withAttr attrBold $ borderWithLabel (str " Select an action (Press enter to confirm): ") $ padTop (Pad 1) $ vBox
            [ if choice st == W then withAttr attrBoldW $ str $ " [w] " ++ (getDescription st 'W') else withAttr attrBold $ str $ " [w] " ++ (getDescription st 'W')
            , if choice st == A then withAttr attrBoldW $ str $ " [a] " ++ (getDescription st 'A') else withAttr attrBold $ str $ " [a] " ++ (getDescription st 'A')
            , if choice st == S then withAttr attrBoldW $ str $ " [s] " ++ (getDescription st 'S') else withAttr attrBold $ str $ " [s] " ++ (getDescription st 'S')
            , if choice st == D then withAttr attrBoldW $ str $ " [d] " ++ (getDescription st 'D') else withAttr attrBold $ str $ " [d] " ++ (getDescription st 'D')
            , hCenter $ str $ " "
            ]
        , withAttr attrRed $ str "Press 'q' to return to main menu."
        ]
      


handleEvent :: UIState -> BrickEvent Name CustomEvent -> EventM Name (Next UIState)
handleEvent Menu (VtyEvent (EvKey (KChar 's') [])) = liftIO initNewGame >>= continue
handleEvent Menu (VtyEvent (EvKey (KChar 'q') [])) = halt Menu
handleEvent Menu _ = continue Menu

handleEvent (StartGame ps) (VtyEvent (EvKey (KChar 'w') [])) = do
  let updatedPS = ps { choice = W}
  continue (StartGame updatedPS) 
  
handleEvent (StartGame ps) (VtyEvent (EvKey (KChar 'a') [])) = do
  let updatedPS = ps { choice = A}
  continue (StartGame updatedPS) 

handleEvent (StartGame ps) (VtyEvent (EvKey (KChar 's') [])) = do
  let updatedPS = ps { choice = S}
  continue (StartGame updatedPS) 

handleEvent (StartGame ps) (VtyEvent (EvKey (KChar 'd') [])) = do
  let updatedPS = ps { choice = D}
  continue (StartGame updatedPS) 

handleEvent (StartGame ps) (VtyEvent (EvKey KEnter [])) = do
  if choice ps == W then do
    let ps' = ps {choice = None} 
    newPS <- liftIO $ updatePlayStatusWithChar 'W' ps'
    if not (alive newPS)
    then continue $ EndGame 0
    else if date newPS >= dateLimit
    then continue $ EndGame 1
    else continue $ StartGame newPS
  else if choice ps == A then do
    let ps' = ps {choice = None} 
    newPS <- liftIO $ updatePlayStatusWithChar 'A' ps'
    if not (alive newPS)
    then continue $ EndGame 0
    else if date newPS >= dateLimit
    then continue $ EndGame 1
    else continue $ StartGame newPS
  else if choice ps == S then do
    let ps' = ps {choice = None} 
    newPS <- liftIO $ updatePlayStatusWithChar 'S' ps'
    if not (alive newPS)
    then continue $ EndGame 0
    else if date newPS >= dateLimit
    then continue $ EndGame 1
    else continue $ StartGame newPS
  else if choice ps == D then do
    let ps' = ps {choice = None} 
    newPS <- liftIO $ updatePlayStatusWithChar 'D' ps'
    if not (alive newPS)
    then continue $ EndGame 0
    else if date newPS >= dateLimit
    then continue $ EndGame 1
    else continue $ StartGame newPS
  else continue (StartGame ps)


handleEvent (StartGame ps) (VtyEvent (EvKey (KChar 'q') [])) = continue Menu
handleEvent st@(StartGame ps) _ = continue st

handleEvent (EndGame _) (VtyEvent (EvKey (KChar 'q') [])) = continue Menu
handleEvent (EndGame res) _ = continue $ EndGame res

-- random generate initial state
randomInitNewGame :: IO UIState
randomInitNewGame = do
  randomWeather <- getRandomWeather
  randomMap <- assignActivitiesToKeys
  return $ StartGame PlayStatus { hunger = 100, 
                                thirsty = 100, 
                                health = 100, 
                                weather = randomWeather, 
                                date = 1,
                                alive = True,
                                activityMap = randomMap,
                                prevActivity = NoActivity,
                                choice = None }