module Activity where

import Brick
import System.Random (randomRIO)

-- Define specific activities for each category
data ForagingActivity = BerryPicking | MushroomForaging | FindWater deriving (Show, Eq)

data HuntingActivity = SmallGameHunting deriving (Show, Eq)

data RestingActivity = StarGazing | Meditating deriving (Show, Eq)

data RandomActivity = BuildTent | MakeFire deriving (Show, Eq)

-- Redefine the Activity data type to include these specific activities
data Activity
  = Foraging ForagingActivity
  | Hunting HuntingActivity
  | Resting RestingActivity
  | Random RandomActivity
  deriving (Show, Eq)

-- Function to describe each specific activity
describeSpecificActivity :: Activity -> String
-- Foraging
describeSpecificActivity (Foraging BerryPicking) = "Gather berries from nearby bushes."
describeSpecificActivity (Foraging MushroomForaging) = "Search for edible mushrooms in the forest."
-- Hunting
describeSpecificActivity (Hunting SmallGameHunting) = "Hunt small animals like rabbits or squirrels."
-- Resting
describeSpecificActivity (Resting StarGazing) = "Look up at the sky and contemplate your survival..."
describeSpecificActivity (Resting Meditating) = "Close your eyes and think..."
-- Random
describeSpecificActivity (Random BuildTent) = "Build a comfy tent."
describeSpecificActivity (Random MakeFire) = "Make a fire to keep you warm."

---- helper for activity effects

type ActivityEffects = (Int, Int, Int) -- (HungerChange, ThirstChange, HealthChange)

activityEffects :: Activity -> IO ActivityEffects
-- Foraging
activityEffects (Foraging BerryPicking) =
  generateRandomEffects (4, 6) (2, 4) (-2, 4)
activityEffects (Foraging MushroomForaging) =
  generateRandomEffects (2, 4) (-3, -1) (-2, 4)
-- Hunting
activityEffects (Hunting SmallGameHunting) =
  generateRandomEffects (9, 11) (-6, -4) (-2, 4)
-- Resting
activityEffects (Resting StarGazing) =
  generateRandomEffects (-5, 0) (-5, 0) (-2, 4)
activityEffects (Resting Meditating) =
  generateRandomEffects (-5, 0) (-5, 0) (-2, 4)
-- Random
activityEffects (Random BuildTent) =
  generateRandomEffects (-11, -9) (-11, -9) (5, 10)
activityEffects (Random MakeFire) =
  generateRandomEffects (-6, -4) (-6, -4) (1, 5)

-- Generate random activity effects
generateRandomEffects :: (Int, Int) -> (Int, Int) -> (Int, Int) -> IO ActivityEffects
generateRandomEffects (hungerMin, hungerMax) (thirstMin, thirstMax) (healthMin, healthMax) = do
  hungerChange <- randomRIO (hungerMin, hungerMax)
  thirstChange <- randomRIO (thirstMin, thirstMax)
  healthChange <- randomRIO (healthMin, healthMax)
  return (hungerChange, thirstChange, healthChange)

-- TODO
-- range of effect on hunger/thirst can be negative, check this value in Game.hs, if negative, print something "poison..."
getRandomForagingActivity :: IO ForagingActivity
getRandomForagingActivity = do
  rand <- randomRIO (1 :: Int, 3 :: Int)
  return $ case rand of
    1 -> BerryPicking
    2 -> MushroomForaging
    3 -> FindWater
    _ -> error "Unexpected random number"

getRandomHuntingActivity :: IO HuntingActivity
getRandomHuntingActivity = do
  rand <- randomRIO (1 :: Int, 1 :: Int)
  return $ case rand of
    1 -> SmallGameHunting
    _ -> error "Unexpected random number"

getRandomRestingActivity :: IO RestingActivity
getRandomRestingActivity = do
  rand <- randomRIO (1 :: Int, 2 :: Int)
  return $ case rand of
    1 -> StarGazing
    2 -> Meditating
    _ -> error "Unexpected random number"

getRandomRandomActivity :: IO RandomActivity
getRandomRandomActivity = do
  rand <- randomRIO (1 :: Int, 2 :: Int)
  return $ case rand of
    1 -> BuildTent
    2 -> MakeFire
    _ -> error "Unexpected random number"