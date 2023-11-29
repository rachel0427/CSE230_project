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

-- Define a type for activity effects
type ActivityEffects = (Int, Int) -- (HungerChange, ThirstChange)

-- Function to get effects of an activity
activityEffects :: Activity -> ActivityEffects
-- Foraging
activityEffects (Foraging BerryPicking) = (5, -3)
activityEffects (Foraging MushroomForaging) = (3, -2)
-- Hunting
activityEffects (Hunting SmallGameHunting) = (10, -5)
-- Resting
activityEffects (Resting StarGazing) = (0, -5)
activityEffects (Resting Meditating) = (0, -5)
-- Random
activityEffects (Random BuildTent) = (-10, -10)
activityEffects (Random MakeFire) = (-5, -5)

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