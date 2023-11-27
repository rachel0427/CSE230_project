module Game where

import System.Random (randomRIO)

-- | Custom data type to represent the game state
data GameState = GameState
    { daysSurvived :: Int
    , healthBar    :: Int
    , hungerBar    :: Int
    , thirstBar    :: Int
    , timeBlocks   :: Int
    , weather      :: Weather
    }

-- | Custom data type to represent the weather
data Weather = Sunny | Rainy | Stormy

-- | Initialize a new game state
initializeGame :: IO GameState
initializeGame = do
    initialWeather <- getRandomWeather
    return $ GameState 0 100 100 100 5 initialWeather

-- | Get a random weather for the game
getRandomWeather :: IO Weather
getRandomWeather = do
    rand <- randomRIO (1, 3)
    case rand of
        1 -> return Sunny
        2 -> return Rainy
        3 -> return Stormy
        _ -> error "Unexpected random number"

-- | Update the game state based on the chosen activity
updateGameState :: GameState -> Activity -> GameState
updateGameState gs activity = newState
  where
    newState = gs
        { daysSurvived = daysSurvived gs + 1
        , timeBlocks = 5
        , healthBar = calculateHealth activity (weather gs) (healthBar gs)
        , hungerBar = calculateHunger activity (weather gs) (hungerBar gs)
        , thirstBar = calculateThirst activity (weather gs) (thirstBar gs)
        }

-- | Custom data type to represent an activity
data Activity = Activity
    { activityName    :: String
    , timeCost        :: Int
    , hungerReward    :: Int
    , thirstReward    :: Int
    , healthPenalty   :: Int
    , probability     :: Double
    }

-- | Calculate the updated health based on the chosen activity
calculateHealth :: Activity -> Weather -> Int -> Int
calculateHealth activity w currentHealth =
    currentHealth - healthPenalty activity - weatherPenalty w

-- | Calculate the updated hunger based on the chosen activity
calculateHunger :: Activity -> Weather -> Int -> Int
calculateHunger activity w currentHunger =
    currentHunger + hungerReward activity - weatherPenalty w

-- | Calculate the updated thirst based on the chosen activity
calculateThirst :: Activity -> Weather -> Int -> Int
calculateThirst activity w currentThirst =
    currentThirst + thirstReward activity - weatherPenalty w

-- | Calculate the weather-based penalty
weatherPenalty :: Weather -> Int
weatherPenalty Sunny = 0
weatherPenalty Rainy = 5
weatherPenalty Stormy = 10