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
  | SeaweedGathering
  | HoneyHunting
  | FlowerForaging
  | FruitTreeGleaning
  | InsectGathering
  | ShellfishGathering
  | BirdeggForaging
  deriving (Show, Eq)

data HuntingActivity
  = DeerHunting
  | FishTrapping
  | BirdHunting
  | InsectCollecting
  | TrapSetting
  | BoarHunting
  | DuckHunting
  | SeabirdHunting
  | RockThrowing
  | BlowpipeHunting
  | BowHunting
  | SpearFishing
  deriving (Show, Eq)

data RestingActivity
  = StarGazing
  | Meditating
  | Daydreaming
  | SunBathing
  | Yoga
  | Sleeping
  | ListenToNature
  | SandCastleBuilding
  | Planning
  | SandPainting
  | Grade
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
  | ExploreUnkonwn
  deriving (Show, Eq)

-- Redefine the Activity data type to include these specific activities
data Activity
  = Foraging ForagingActivity
  | Hunting HuntingActivity
  | Resting RestingActivity
  | Random RandomActivity
  | NoActivity
  deriving (Show, Eq)

-- Function to describe each specific activity
activityText :: Activity -> String
-- Foraging
activityText (Foraging BerryPicking) = "Gather berries from nearby bushes."
activityText (Foraging MushroomForaging) = "Search for edible mushrooms in the forest."
activityText (Foraging FindWater) = "Search for water, the source of life!"
activityText (Foraging HerbGathering) = "Gather herbs from nearby forest. They may heal you!"
activityText (Foraging NutCollecting) = "Collect nuts from nearby forest. Might anger the squirrels."
activityText (Foraging RootDigging) = "Dig up edible roots."
activityText (Foraging SeaweedGathering) = "Look for seaweed along the coast."
activityText (Foraging HoneyHunting) = "Search for delicious and nutritous honey. Look out for bees!"
activityText (Foraging FlowerForaging) = "Find edible flowers. Might have healing properties."
activityText (Foraging FruitTreeGleaning) = "Look for fruit trees. Shake em up!"
activityText (Foraging InsectGathering) = "Capture some poor bugs for dinner. Ewww but edible."
activityText (Foraging ShellfishGathering) = "Find some yummy shell fish for dinner."
activityText (Foraging BirdeggForaging) = "Collect bird eggs from nests. Sorry baby birds."
-- Hunting
activityText (Hunting DeerHunting) = "Go hunting for deers at risk of injury."
activityText (Hunting FishTrapping) = "Try to trap some fish by the river."
activityText (Hunting BirdHunting) = "Try to catch some birds for dinner."
activityText (Hunting InsectCollecting) = "Catch some insects, not optimal but edible."
activityText (Hunting TrapSetting) = "Set up a trap. Who knows what you'll get."
activityText (Hunting BoarHunting) = "Go hunting for boars at risk of injury."
activityText (Hunting DuckHunting) = "Catch a fat wild duck. Quack quack."
activityText (Hunting SeabirdHunting) = "Try catching seabirds along the coastline."
activityText (Hunting RockThrowing) = "Throwing CAREFULLY aimed rock at small game."
activityText (Hunting BlowpipeHunting) = "Hunt small animals with handcrafted blowpipe."
activityText (Hunting BowHunting) = "Craft a bow and arrows. Safer choice for larger prey!"
activityText (Hunting SpearFishing) = "Stab a fish with your spear near the coral reefs. Ouch!"
-- Resting
activityText (Resting StarGazing) = "Look up at the sky and contemplate your survival..."
activityText (Resting Meditating) = "Close your eyes and think..."
activityText (Resting Daydreaming) = "Take some time to daydream what it feels like to be at home."
activityText (Resting SunBathing) = "Absorb energy from sunbathing."
activityText (Resting Yoga) = "Feeling Down? Do some Yoga."
activityText (Resting Sleeping) = "Sleeping is always the most effective way to refill and recharge."
activityText (Resting ListenToNature) = "Sit silently and listen to the sounds of nature."
activityText (Resting SandCastleBuilding) = "Get on the beach and build a sand castle."
activityText (Resting Planning) = "Lay down and plan your next days."
activityText (Resting SandPainting) = "Paint a picture on the sand."
activityText (Resting Grade) = "Calculate your CSE230 final grade."
-- Random
activityText (Random BuildTent) = "Build a comfy tent."
activityText (Random MakeFire) = "Make a fire to keep you warm."
activityText (Random MapMaking) = "Draw a map to learn more about your island."
activityText (Random ShelterImproving) = "A better shelter can probably last you through bad weather."
activityText (Random WaterPurifying) = "Drinking purified water is the key to survival!"
activityText (Random FortuneTelling) = "Predict your future..."
activityText (Random NightPatrolling) = "Make sure no threats on your survival territory."
activityText (Random SignalingForHelp) = "Making a huge SOS should improve your chances to be rescued..."
activityText (Random ExploreUnkonwn) = "Get out of your comfort zone and explore the wider nature."

-- Default starting activity
activityText NoActivity = "None yet. Pick an activity to start."

---- helper for activity effects

type ActivityEffects = (Int, Int, Int) -- (HungerChange, ThirstChange, HealthChange)

activityEffects :: Activity -> IO ActivityEffects
-- Foraging
activityEffects (Foraging BerryPicking) =
  generateRandomEffects (5, 10) (5, 10) (-10, 10)
activityEffects (Foraging MushroomForaging) =
  generateRandomEffects (5, 10) (0, 5) (-10, 10)
activityEffects (Foraging FindWater) =
  generateRandomEffects (2, 4) (-3, 10) (-5, 5)
activityEffects (Foraging HerbGathering) =
  generateRandomEffects (2, 4) (5, 7) (-5, 5)
activityEffects (Foraging NutCollecting) =
  generateRandomEffects (2, 10) (-3, -1) (0, 5)
activityEffects (Foraging RootDigging) =
  generateRandomEffects (3, 10) (-1, 5) (0, 10)
activityEffects (Foraging SeaweedGathering) =
  generateRandomEffects (3, 10) (-5, -1) (0, 4)
activityEffects (Foraging HoneyHunting) =
  generateRandomEffects (3, 10) (-3, -1) (-15, 10)
activityEffects (Foraging FlowerForaging) =
  generateRandomEffects (2, 10) (-3, 5) (-2, 4)
activityEffects (Foraging FruitTreeGleaning) =
  generateRandomEffects (5, 15) (5, 10) (-2, 5)
activityEffects (Foraging InsectGathering) =
  generateRandomEffects (2, 6) (-3, -1) (-3, 5)
activityEffects (Foraging ShellfishGathering) =
  generateRandomEffects (3, 10) (-3, -1) (0, 5)
activityEffects (Foraging BirdeggForaging) =
  generateRandomEffects (3, 10) (-3, -1) (3, 7)
-- Hunting
activityEffects (Hunting DeerHunting) =
  generateRandomEffects (20, 30) (-15, 5) (-50, 30)
activityEffects (Hunting FishTrapping) =
  generateRandomEffects (8, 12) (-10, -5) (-5, 10)
activityEffects (Hunting BirdHunting) =
  generateRandomEffects (10, 15) (-6, -4) (-5, 10)
activityEffects (Hunting InsectCollecting) =
  generateRandomEffects (3, 7) (-6, -4) (-10, 10)
activityEffects (Hunting TrapSetting) =
  generateRandomEffects (3, 20) (-6, -4) (-10, 10)
activityEffects (Hunting BoarHunting) =
  generateRandomEffects (20, 30) (-15, 5) (-50, 20)
activityEffects (Hunting DuckHunting) =
  generateRandomEffects (10, 20) (-10, 5) (-4, 10)
activityEffects (Hunting SeabirdHunting) =
  generateRandomEffects (10, 15) (-10, 5) (-5, 10)
activityEffects (Hunting RockThrowing) =
  generateRandomEffects (6, 12) (-5, 5) (-8, 8)
activityEffects (Hunting BlowpipeHunting) =
  generateRandomEffects (5, 15) (-5, 5) (-5, 10)
activityEffects (Hunting BowHunting) =
  generateRandomEffects (5, 20) (-10, 5) (-5, 10)
activityEffects (Hunting SpearFishing) =
  generateRandomEffects (8, 12) (-6, -4) (-5, 10)
-- Resting
activityEffects (Resting StarGazing) =
  generateRandomEffects (-15, -5) (-10, -5) (5, 10)
activityEffects (Resting Meditating) =
  generateRandomEffects (-15, -5) (-10, -5) (5, 10)
activityEffects (Resting Daydreaming) =
  generateRandomEffects (-15, -5) (-10, -5) (5, 10)
activityEffects (Resting SunBathing) =
  generateRandomEffects (-15, -5) (-15, -5) (-5, 5)
activityEffects (Resting Yoga) =
  generateRandomEffects (-15, -5) (-10, -5) (0, 5)
activityEffects (Resting Sleeping) =
  generateRandomEffects (-10, 0) (-5, 0) (5, 10)
activityEffects (Resting ListenToNature) =
  generateRandomEffects (-15, -5) (-10, -5) (0, 5)
activityEffects (Resting SandCastleBuilding) =
  generateRandomEffects (-15, -5) (-10, -5) (5, 10)
activityEffects (Resting Planning) =
  generateRandomEffects (-15, 0) (-10, -5) (5, 10)
activityEffects (Resting SandPainting) =
  generateRandomEffects (-15, -5) (-10, -5) (5, 10)
activityEffects (Resting Grade) =
  generateRandomEffects (-20, -5) (-10, -5) (-20, -5)
-- Random
activityEffects (Random BuildTent) =
  generateRandomEffects (-11, -9) (-11, -9) (5, 10)
activityEffects (Random MakeFire) =
  generateRandomEffects (-6, -4) (-6, -4) (1, 5)
activityEffects (Random MapMaking) =
  generateRandomEffects (-11, -9) (-11, -9) (5, 10)
activityEffects (Random ShelterImproving) =
  generateRandomEffects (-10, -4) (-6, -4) (5, 10)
activityEffects (Random WaterPurifying) =
  generateRandomEffects (-5, -0) (-1, 10) (0, 5)
activityEffects (Random FortuneTelling) =
  generateRandomEffects (-10, -5) (-5, -3) (1, 5)
activityEffects (Random NightPatrolling) =
  generateRandomEffects (-10, -5) (-10, -5) (5, 10)
activityEffects (Random SignalingForHelp) =
  generateRandomEffects (-20, -5) (-10, -10) (-20, 10)
activityEffects (Random ExploreUnkonwn) =
  generateRandomEffects (-10, 10) (-10, 10) (-5, 5)

-- Default starting activity
activityEffects NoActivity = 
  generateRandomEffects (0,0) (0,0) (0,0)

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
  rand <- randomRIO (1 :: Int, 13 :: Int)
  return $ case rand of
    1 -> BerryPicking
    2 -> MushroomForaging
    3 -> FindWater
    4 -> HerbGathering
    5 -> NutCollecting
    6 -> RootDigging
    7 -> SeaweedGathering
    8 -> HoneyHunting
    9 -> FlowerForaging
    10 -> FruitTreeGleaning
    11 -> InsectGathering
    12 -> ShellfishGathering
    13 -> BirdeggForaging
    _ -> error "Unexpected random number"

getRandomHuntingActivity :: IO HuntingActivity
getRandomHuntingActivity = do
  rand <- randomRIO (1 :: Int, 12 :: Int)
  return $ case rand of
    1 -> DeerHunting
    2 -> FishTrapping
    3 -> BirdHunting
    4 -> InsectCollecting
    5 -> TrapSetting
    6 -> BoarHunting
    7 -> DuckHunting
    8 -> SeabirdHunting
    9 -> RockThrowing
    10 -> BlowpipeHunting
    11 -> BowHunting
    12 -> SpearFishing
    _ -> error "Unexpected random number"

getRandomRestingActivity :: IO RestingActivity
getRandomRestingActivity = do
  rand <- randomRIO (1 :: Int, 11 :: Int)
  return $ case rand of
    1 -> StarGazing
    2 -> Meditating
    3 -> Daydreaming
    4 -> SunBathing
    5 -> Yoga
    6 -> Sleeping
    7 -> ListenToNature
    8 -> SandCastleBuilding
    9 -> Planning
    10 -> SandPainting
    11 -> Grade
    _ -> error "Unexpected random number"

getRandomRandomActivity :: IO RandomActivity
getRandomRandomActivity = do
  rand <- randomRIO (1 :: Int, 9 :: Int)
  return $ case rand of
    1 -> BuildTent
    2 -> MakeFire
    3 -> MapMaking
    4 -> ShelterImproving
    5 -> WaterPurifying
    6 -> FortuneTelling
    7 -> NightPatrolling
    8 -> SignalingForHelp
    9 -> ExploreUnkonwn
    _ -> error "Unexpected random number"
