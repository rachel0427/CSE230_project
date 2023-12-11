module Types (module Types) where
import Brick      
import Data.Map as M
import Activity

-- Define the weather data type
data Weather = Sunny | Rainy | Stormy deriving (Show, Eq)

data PlayStatus = PlayStatus
                    { hunger  :: Int   -- range [0, 100]
                    , thirsty :: Int   -- range [0, 100]
                    , health  :: Int   -- range [0, 100]
                    , weather :: Weather
                    , date :: Int      -- range [1, 10]
                    , alive :: Bool
                    , activityMap :: M.Map Char Activity
                    , prevActivity :: Activity
                    } deriving (Show, Eq)

data Option = Option 
                {
                      op1 :: String 
                    , op2 :: String
                    , op3 :: String
                    , op4 :: String

                }

demoOption :: Option      
demoOption = Option { op1 = "Search for edible mushrooms in the forest." 
             , op2 = "Hunt small animals like rabbits or squirrels."
             , op3 = "Look up at the sky and contemplate your survival..."
             , op4 = "Make a fire to keep you warm."}

data Art = Art {
                    sunny :: String 
                  , rainy :: String
                  , cloudy :: String
                }

fixArt = Art {sunny = "                   \\       /            _\\/_\n" ++
                     ".-'-.              //o\\  _\\/_\n" ++
  "_  ___  __  _ --_ /     \\ _--_ __  __ _ | __/o\\\\ _\n" ++
"=-=-_=-=-_=-=_=-_= -=======- = =-=_=-=_,-'|\"'\"\"-|-,_\n" ++ 
"=- _=-=-_=- _=-= _--=====- _=-=_-_,-\"          |",
rainy = "      __   _\n" ++
    "_(  )_( )_\n" ++
   "(_   _    _)\n" ++
  "/ /(_) (__)\n" ++
 "/ / / / / /\n" ++
"/ / / / / /",
cloudy = "   __   _\n" ++
" _(  )_( )_\n" ++
"(_   _    _)\n" ++
"  (_) (__)\n"}