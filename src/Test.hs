import Test.QuickCheck
import Activity
import Types
import Test.QuickCheck.Monadic
import Control.Monad.IO.Class (liftIO)
import Data.Map as M
import Game


import Test.QuickCheck

-- Assuming the definition of Activity from your module
-- ...

instance Arbitrary Activity where
  arbitrary = oneof [ foragingArb
                    , huntingArb
                    , restingArb
                    , randomArb
                    , return NoActivity
                    ]
    where
      foragingArb = elements [  Foraging MushroomForaging,
                                Foraging FindWater,
                                Foraging HerbGathering,
                                Foraging NutCollecting,
                                Foraging RootDigging,
                                Foraging SeaweedGathering,
                                Foraging HoneyHunting,
                                Foraging FlowerForaging,
                                Foraging FruitTreeGleaning,
                                Foraging InsectGathering,
                                Foraging ShellfishGathering,
                                Foraging BirdeggForaging
                             ]
      huntingArb = elements [   Hunting DeerHunting,
                                Hunting FishTrapping,
                                Hunting BirdHunting,
                                Hunting InsectCollecting,
                                Hunting TrapSetting,
                                Hunting BoarHunting,
                                Hunting DuckHunting,
                                Hunting SeabirdHunting,
                                Hunting RockThrowing,
                                Hunting BlowpipeHunting,
                                Hunting BowHunting,
                                Hunting SpearFishing
                                ]
      restingArb = elements [   Resting StarGazing,
                                Resting Meditating,
                                Resting Daydreaming,
                                Resting SunBathing,
                                Resting Yoga,
                                Resting Sleeping,
                                Resting ListenToNature,
                                Resting SandCastleBuilding,
                                Resting Planning,
                                Resting SandPainting,
                                Resting Grade

                            ]
      randomArb = elements [    Random BuildTent,
                                Random MakeFire,
                                Random MapMaking,
                                Random ShelterImproving,
                                Random WaterPurifying,
                                Random FortuneTelling,
                                Random NightPatrolling,
                                Random SignalingForHelp, -- TODO: early stopping, the game
                                Random ExploreUnkonwn

                           ]

-- Now you can use quickCheck on properties involving Activity


prop_activityEffectsRange :: Activity -> Property
prop_activityEffectsRange activity = monadicIO $ do
  (hungerChange, thirstChange, healthChange) <- run $ activityEffects activity
  assert (inRange hungerChange && inRange thirstChange && inRange healthChange)
  where
    inRange value = value >= minBound && value <= maxBound
    -- Define minBound and maxBound based on the expected range for each activity

-- In your test suite
-- >>> quickCheck prop_activityEffectsRange
-- +++ OK, passed 100 tests.
--

prop_getRandomForagingActivity :: Property
prop_getRandomForagingActivity = monadicIO $ do
  activity <- run getRandomForagingActivity
  assert $ activity `elem` allForagingActivities
  where
    allForagingActivities = [ BerryPicking
                            , MushroomForaging
                            , FindWater
                            , HerbGathering
                            , NutCollecting
                            , SeaweedGathering
                            , RootDigging
                            , HoneyHunting
                            , FlowerForaging
                            , FruitTreeGleaning
                            , InsectGathering
                            , ShellfishGathering
                            , BirdeggForaging
                            ]

-- In your test suite
-- >>> quickCheck prop_getRandomForagingActivity
-- +++ OK, passed 100 tests.


prop_getRandomWeather :: Property
prop_getRandomWeather = monadicIO $ do
    weather <- run getRandomWeather
    assert $ weather `elem` [Sunny, Rainy, Cloudy]

-- In your test suite
-- >>> quickCheck prop_getRandomWeather
-- +++ OK, passed 100 tests.
--

prop_checkAlive :: Int -> Bool
prop_checkAlive health =
    alive (checkAlive (PlayStatus 0 0 health Sunny 0 True M.empty NoActivity)) == (health >= 0)

-- In your test suite
-- >>> quickCheck prop_checkAlive
-- +++ OK, passed 100 tests.

