import Test.QuickCheck
import Activity
import Types
import Test.QuickCheck.Monadic

prop_activityEffectsRange :: Activity -> Property
prop_activityEffectsRange activity = monadicIO $ do
  (hungerChange, thirstChange, healthChange) <- run $ activityEffects activity
  assert (inRange hungerChange && inRange thirstChange && inRange healthChange)
  where
    inRange value = value >= minBound && value <= maxBound
    -- Define minBound and maxBound based on the expected range for each activity

-- In your test suite
-- >>> quickCheck prop_activityEffectsRange
-- (Error while loading modules for evaluation)
-- [5 of 5] Compiling Main             ( /workspaces/CSE230_project/src/Test.hs, interpreted )
-- <BLANKLINE>
-- /workspaces/CSE230_project/src/Test.hs:14:1-36: error:
--     Parse error: module header, import declaration
--     or top-level declaration expected.
-- Failed, four modules loaded.
--

