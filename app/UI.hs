{-# LANGUAGE OverloadedStrings #-}

module UI where

import Graphics.Vty
import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center, vCenter)
import Brick.Widgets.Core (str)
import Brick.Widgets.Core ( (<+>) )

import qualified Game as Game

-- | Custom data type to represent the UI state
data UIState = UIState
    { game   :: Game.GameState
    , screen :: Screen
    }

-- | Custom data type to represent the different screens in the UI
data Screen = MainMenu | NewGame

-- | Custom data type to represent the UI events
data UIEvent = UINewGame | UIExit

-- | Define the Brick application
app :: App UIState UIEvent
app = App
    { appDraw         = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return
    , appAttrMap      = const $ attrMap defAttr []
    }

-- | Define the initial game state and UI state
initialGameState :: Game.GameState
initialGameState = Game.GameState 0 100 100 100 5 Game.Sunny

initialUIState :: UIState
initialUIState = UIState initialGameState MainMenu

-- | Draw the UI based on the current UI state
drawUI :: UIState -> [Widget UIState]
drawUI uiState =
    case screen uiState of
        MainMenu -> [center mainMenuWidget]
        NewGame  -> [center newGameWidget]

-- | Define the main menu widget with buttons
mainMenuWidget :: Widget UIState
mainMenuWidget =
    vBox [str "Survival Game"
         , str " "
         , button "Start New Game" UINewGame
         , str " "
         , button "Exit" UIExit
         ]

-- | Define the new game screen widget
newGameWidget :: Widget UIState
newGameWidget =
    vBox [str "New Game Screen"
         , str " "
         , str "Press 'q' to go back to the main menu."
         ]

-- | Define a button with a given label and event
button :: String -> UIEvent -> Widget UIState
button label event = clickable (const event) $ hCenter $ str ("[" <> label <> "]")

-- | Handle events based on the current UI state
handleEvent :: UIState -> BrickEvent UIState UIEvent -> EventM UIState (Next UIState)
handleEvent uiState (VtyEvent (EvKey KEnter [])) = handleEnterKey uiState
handleEvent uiState (VtyEvent (EvKey (KChar 'q') [])) = handleQKey uiState
handleEvent uiState _ = continue uiState

-- | Handle 'Enter' key events
handleEnterKey :: UIState -> EventM UIState (Next UIState)
handleEnterKey uiState =
    case screen uiState of
        MainMenu -> handleMainMenuEnter uiState
        NewGame  -> continue uiState

-- | Handle 'q' key events
handleQKey :: UIState -> EventM UIState (Next UIState)
handleQKey uiState =
    case screen uiState of
        NewGame -> continue uiState { screen = MainMenu }
        _       -> halt uiState

-- | Handle 'Enter' key events on the main menu
handleMainMenuEnter :: UIState -> EventM UIState (Next UIState)
handleMainMenuEnter uiState =
    case screen uiState of
        MainMenu -> handleMainMenuSelection uiState
        _        -> continue uiState

-- | Handle main menu item selection
handleMainMenuSelection :: UIState -> EventM UIState (Next UIState)
handleMainMenuSelection uiState =
    case getSelectedEvent (currentAttrName $ overrideAttr uiState) of
        Just UINewGame -> continue uiState { screen = NewGame }
        Just UIExit     -> halt uiState
        _               -> continue uiState

-- | Get the selected event based on the current attribute name
getSelectedEvent :: AttrName -> Maybe UIEvent
getSelectedEvent attrName
    | attrName == "selected" = Just UINewGame
    | attrName == "normal"   = Just UIExit
    | otherwise              = Nothing

-- | Override the attribute based on the current UI state
overrideAttr :: UIState -> AttrName
overrideAttr uiState =
    case screen uiState of
        MainMenu -> "selected"
        _        -> "normal"
