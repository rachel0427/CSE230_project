module Game (module Game) where

-- import Activity (Activity, activityEffects)
import Activity
import Data.Map as M
import System.Random (randomRIO)
import Types

-- | Custom data type to represent the game state
-- data PlayStatus = PlayStatus
--   { daysSurvived :: Int,
--     healthBar :: Int,
--     hungerBar :: Int,
--     thirstBar :: Int,
--     timeBlocks :: Int,
--     weather1 :: Weather,
--     alive :: Bool
--   }

-- -- | Custom data type to represent the weather
-- data Weather = Sunny | Rainy | Stormy

-- | Initialize a new game state
-- initializeGame :: IO PlayStatus
-- initializeGame = do
--   initialWeather <- getRandomWeather
--   return $ PlayStatus 100 100 100 initialWeather 0 True

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
updatePlayStatus :: PlayStatus -> Activity -> IO PlayStatus
updatePlayStatus gs activity = do
  (hungerChange, thirstChange, healthChange) <- activityEffects activity
  -- Apply changes to PlayStatus
  let newHunger = max 0 (min 100 (hunger gs + hungerChange))
  let newThirst = max 0 (min 100 (thirsty gs + thirstChange))
  let newHealth = max 0 (min 100 (health gs + healthChange))
  let newAlive = newHealth > 0
  return gs {hunger = newHunger, thirsty = newThirst, health = newHealth, alive = newAlive}

-- Check whether player is still alive
checkAlive :: PlayStatus -> PlayStatus
checkAlive gs =
  gs {alive = health gs >= 0}

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

-- Function to map keys to activities
assignActivitiesToKeys :: IO (M.Map Char Activity)
assignActivitiesToKeys = do
  foragingActivity <- getRandomForagingActivity
  huntingActivity <- getRandomHuntingActivity
  restingActivity <- getRandomRestingActivity
  randomActivity <- getRandomRandomActivity

  let activityMap =
        M.fromList
          [ ('W', Foraging foragingActivity),
            ('A', Hunting huntingActivity),
            ('S', Resting restingActivity),
            ('D', Random randomActivity)
          ]
  return activityMap

-- Function to update PlayStatus based on the user's input
updatePlayStatusWithChar :: Char -> PlayStatus -> IO PlayStatus
updatePlayStatusWithChar char playStatus@(PlayStatus _ _ _ _ _ _ activityMap) =
  case M.lookup char activityMap of
    Just activity -> do
      (hungerChange, thirstChange, healthChange) <- activityEffects activity
      return $ applyChanges playStatus hungerChange thirstChange healthChange
    Nothing -> return playStatus -- Return the original status if the character doesn't match any activity

applyChanges :: PlayStatus -> Int -> Int -> Int -> PlayStatus
applyChanges playStatus hungerChange thirstChange healthChange =
  let -- Calculate new values while ensuring they stay within the 0-100 range
      newHunger = max 0 (min 100 (hunger playStatus + hungerChange))
      newThirst = max 0 (min 100 (thirsty playStatus + thirstChange))
      newHealth = max 0 (min 100 (health playStatus + healthChange))

      -- Update the 'alive' status based on the new health value
      newAlive = newHealth > 0
   in -- Update and return the new PlayStatus

      playStatus
        { hunger = newHunger,
          thirsty = newThirst,
          health = newHealth,
          alive = newAlive
        }

getDescription :: PlayStatus -> Char -> String
getDescription (PlayStatus _ _ _ _ _ _ activityMap) char =
  case M.lookup char activityMap of
    Just activity -> activityText activity
    Nothing -> "No activity found for this key"
