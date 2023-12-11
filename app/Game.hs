module Game (module Game) where

-- import Activity (Activity, activityEffects)
import Activity
import Control.Monad.IO.Class (liftIO)
import Data.Map as M
import System.Random (randomRIO)
import Types

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
-- updatePlayStatus :: PlayStatus -> Activity -> IO PlayStatus
-- updatePlayStatus gs activity = do
--   (hungerChange, thirstChange, healthChange) <- activityEffects activity
--   -- Apply changes to PlayStatus
--   let newHunger = max 0 (min 100 (hunger gs + hungerChange))
--   let newThirst = max 0 (min 100 (thirsty gs + thirstChange))
--   let newHealth = max 0 (min 100 (health gs + healthChange))
--   let newAlive = newHealth > 0
--   return gs {hunger = newHunger, thirsty = newThirst, health = newHealth, alive = newAlive}


-- Check whether player is still alive
checkAlive :: PlayStatus -> PlayStatus
checkAlive gs =
  gs {alive = health gs >= 0}

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

-- Calculate the weather-based penalty (Hunger, Thirst)
weatherPenalty :: Weather -> (Int, Int)
weatherPenalty Sunny = (-10, -10)
weatherPenalty Rainy = (-10, -5) -- Assuming no thirst change for Rainy
weatherPenalty Stormy = (-10, -5) -- Assuming equal hunger and thirst change for Stormy

-- Function to update PlayStatus based on the user's input
updatePlayStatusWithChar :: Char -> PlayStatus -> IO PlayStatus

updatePlayStatusWithChar char playStatus@(PlayStatus _ _ _ _ _ _ activityMap _) =
  case M.lookup char activityMap of
    Just activity -> do
      (hungerChange, thirstChange, healthChange) <- activityEffects activity
      pt <- applyChanges playStatus hungerChange thirstChange healthChange activity
      return pt
    Nothing -> return playStatus -- Return the original status if the character doesn't match any activity

applyChanges :: PlayStatus -> Int -> Int -> Int -> Activity -> IO PlayStatus
applyChanges playStatus hungerChange thirstChange healthChange chosenActivity = do
  newActivityMap <- liftIO assignActivitiesToKeys
  let
    newHunger = max 0 (min 100 (hunger playStatus + hungerChange))
    newThirst = max 0 (min 100 (thirsty playStatus + thirstChange))
    newHealth = max 0 (min 100 (health playStatus + healthChange))
    newAlive = newHealth > 0

  return $
    playStatus
      { hunger = newHunger,
        thirsty = newThirst,
        health = newHealth,
        alive = newAlive,
        activityMap = newActivityMap,
        prevActivity = chosenActivity
      }


getDescription :: PlayStatus -> Char -> String
getDescription (PlayStatus _ _ _ _ _ _ activityMap _) char =
  case M.lookup char activityMap of
    Just activity -> activityText activity
    Nothing -> "No activity found for this key"
