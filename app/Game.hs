module Game where

import Activity
import Activity (Activity, activityEffects)
import System.Random (randomRIO)

-- | Custom data type to represent the game state
data GameState = GameState
  { daysSurvived :: Int,
    healthBar :: Int,
    hungerBar :: Int,
    thirstBar :: Int,
    timeBlocks :: Int,
    weather :: Weather
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
  rand <- randomRIO (1 :: Int, 3 :: Int)
  case rand of
    1 -> return Sunny
    2 -> return Rainy
    3 -> return Stormy
    _ -> error "Unexpected random number"

-- Update the game state based on the chosen activity
updateGameState :: GameState -> Activity -> IO GameState
updateGameState gs activity = do
  let (hungerChange, thirstChange) = activityEffects activity
  -- Apply changes to GameState
  let newHunger = max 0 (min 100 (hungerBar gs + hungerChange))
  let newThirst = max 0 (min 100 (thirstBar gs + thirstChange))
  return gs {hungerBar = newHunger, thirstBar = newThirst}

-- Rest of the Game.hs code
{-
-- | Custom data type to represent an activity
data ActivityOutcome = ActivityOutcome
  { activityName :: String,
    timeCost :: Int,
    hungerReward :: Int,
    thirstReward :: Int,
    healthPenalty :: Int,
    probability :: Double
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
-}

-- | Calculate the weather-based penalty
weatherPenalty :: Weather -> Int
weatherPenalty Sunny = 0
weatherPenalty Rainy = 5
weatherPenalty Stormy = 10

{-
-- Example function extension
calculateHealth :: Activity -> Weather -> Int -> Int
calculateHealth activity w currentHealth = case activity of
  Foraging -> currentHealth - 10 - weatherPenalty w
  CollectingWater -> currentHealth - 5 - weatherPenalty w
  BuildingShelter -> currentHealth - 15 - weatherPenalty w
  Resting -> currentHealth + 10
  Exploring -> currentHealth - 20 - weatherPenalty w
-}
