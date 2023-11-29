module Activity where

import Brick

-- Define specific activities for each category
data ForagingActivity = BerryPicking | MushroomForaging deriving (Show, Eq)

data HuntingActivity = SmallGameHunting deriving (Show, Eq)

-- Redefine the Activity data type to include these specific activities
data Activity = Foraging ForagingActivity | Hunting HuntingActivity deriving (Show, Eq)

-- Function to describe each specific activity
describeSpecificActivity :: Activity -> String
describeSpecificActivity (Foraging BerryPicking) = "Gather berries from nearby bushes."
describeSpecificActivity (Foraging MushroomForaging) = "Search for edible mushrooms in the forest."
describeSpecificActivity (Hunting SmallGameHunting) = "Hunt small animals like rabbits or squirrels."

-- Define a type for activity effects
type ActivityEffects = (Int, Int) -- (HungerChange, ThirstChange)

-- Function to get effects of an activity
activityEffects :: Activity -> ActivityEffects
activityEffects (Foraging BerryPicking) = (5, -3)
activityEffects (Foraging MushroomForaging) = (3, -2)
activityEffects (Hunting SmallGameHunting) = (10, -5)
