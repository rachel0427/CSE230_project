module Activity where

import Brick
import System.Random (randomRIO)

-- Define specific activities for each category
data ForagingActivity
  = BerryPicking
  | MushroomForaging
  | FindWater
  | HerbGathering
  | NutCollecting
  | RootDigging
  deriving (Show, Eq)

data HuntingActivity
  = DeerHunting
  | FishTrapping
  | BirdHunting
  | InsectCollecting
  | TrapSetting
  deriving (Show, Eq)

data RestingActivity
  = StarGazing
  | Meditating
  | Daydreaming
  | SunBathing
  | Yoga
  | Sleeping
  deriving (Show, Eq)

data RandomActivity
  = BuildTent
  | MakeFire
  | MapMaking
  | ShelterImproving
  | WaterPurifying
  | FortuneTelling
  | NightPatrolling
  | SignalingForHelp -- TODO: early stopping the game
  deriving (Show, Eq)

-- Redefine the Activity data type to include these specific activities
data Activity
  = Foraging ForagingActivity
  | Hunting HuntingActivity
  | Resting RestingActivity
  | Random RandomActivity
  deriving (Show, Eq)

-- Function to describe each specific activity
activityText :: Activity -> String
-- Foraging
activityText (Foraging BerryPicking) = "Gather berries from nearby bushes."
activityText (Foraging MushroomForaging) = "Search for edible mushrooms in the forest."
activityText (Foraging FindWater) = "Search for water, the source of life!"
activityText (Foraging HerbGathering) = "Gather herbs from nearby forest. They may heal you!"
activityText (Foraging NutCollecting) = "Collect nuts from nearby forest. Might anger the squirrels."
activityText (Foraging RootDigging) = "Dig up roots. Breakfast is served."
-- Hunting
activityText (Hunting DeerHunting) = "Go hunting for deers at risk of injury."
activityText (Hunting FishTrapping) = "Try to trap some fish by the river."
activityText (Hunting BirdHunting) = "Try to catch some birds for dinner."
activityText (Hunting InsectCollecting) = "Catch some insects, not optimal but edible."
activityText (Hunting TrapSetting) = "Set up a trap. Who knows what you'll get."
-- Resting
activityText (Resting StarGazing) = "Look up at the sky and contemplate your survival..."
activityText (Resting Meditating) = "Close your eyes and think..."
activityText (Resting Daydreaming) = "Take some time to daydream what it feels like to be at home"
activityText (Resting SunBathing) = "Absorb energy form sunbathing"
activityText (Resting Yoga) = "Feeling Down? Do some Yoga"
activityText (Resting Sleeping) = "Sleeping is always the most effective way to refill and recharge"
-- Random
activityText (Random BuildTent) = "Build a comfy tent."
activityText (Random MakeFire) = "Make a fire to keep you warm."
activityText (Random MapMaking) = "Draw a map to learn more about your island."
activityText (Random ShelterImproving) = "A better shelter can probably last you through bad weather."
activityText (Random WaterPurifying) = "Drinking purified water is the key to survival!"
activityText (Random FortuneTelling) = "Predict your future..."
activityText (Random NightPatrolling) = "Make sure no threats on your survival territory."
activityText (Random SignalingForHelp) = "Making a huge SOS should improve your chances to be rescued..."

---- helper for activity effects

type ActivityEffects = (Int, Int, Int) -- (HungerChange, ThirstChange, HealthChange)

activityEffects :: Activity -> IO ActivityEffects
-- Foraging
activityEffects (Foraging BerryPicking) =
  generateRandomEffects (5, 10) (5, 10) (-10, 10)
activityEffects (Foraging MushroomForaging) =
  generateRandomEffects (5, 10) (0, 5) (-10, 10)
activityEffects (Foraging FindWater) =
  generateRandomEffects (2, 4) (-3, -1) (-5, 5)
activityEffects (Foraging HerbGathering) =
  generateRandomEffects (2, 4) (-3, -1) (-2, 4)
activityEffects (Foraging NutCollecting) =
  generateRandomEffects (2, 4) (-3, -1) (-2, 4)
activityEffects (Foraging RootDigging) =
  generateRandomEffects (2, 4) (-3, -1) (-2, 4)
-- Hunting
activityEffects (Hunting DeerHunting) =
  generateRandomEffects (20, 30) (-15, -5) (-50, 50)
activityEffects (Hunting FishTrapping) =
  generateRandomEffects (10, 15) (-10, -5) (0, 10)
activityEffects (Hunting BirdHunting) =
  generateRandomEffects (10, 15) (-6, -4) (0, 10)
activityEffects (Hunting InsectCollecting) =
  generateRandomEffects (10, 15) (-6, -4) (0, 10)
activityEffects (Hunting TrapSetting) =
  generateRandomEffects (10, 15) (-6, -4) (0, 10)
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
  rand <- randomRIO (1 :: Int, 6 :: Int)
  return $ case rand of
    1 -> BerryPicking
    2 -> MushroomForaging
    3 -> FindWater
    4 -> HerbGathering
    5 -> NutCollecting
    6 -> RootDigging
    _ -> error "Unexpected random number"

getRandomHuntingActivity :: IO HuntingActivity
getRandomHuntingActivity = do
  rand <- randomRIO (1 :: Int, 5 :: Int)
  return $ case rand of
    1 -> DeerHunting
    2 -> FishTrapping
    3 -> BirdHunting
    4 -> InsectCollecting
    5 -> TrapSetting
    _ -> error "Unexpected random number"

getRandomRestingActivity :: IO RestingActivity
getRandomRestingActivity = do
  rand <- randomRIO (1 :: Int, 6 :: Int)
  return $ case rand of
    1 -> StarGazing
    2 -> Meditating
    3 -> Daydreaming
    4 -> SunBathing
    5 -> Yoga
    6 -> Sleeping
    _ -> error "Unexpected random number"

getRandomRandomActivity :: IO RandomActivity
getRandomRandomActivity = do
  rand <- randomRIO (1 :: Int, 8 :: Int)
  return $ case rand of
    1 -> BuildTent
    2 -> MakeFire
    3 -> MapMaking
    4 -> ShelterImproving
    5 -> WaterPurifying
    6 -> FortuneTelling
    7 -> NightPatrolling
    8 -> SignalingForHelp
    _ -> error "Unexpected random number"
