module Types (module Types) where
import Brick      

-- Define the weather data type
data Weather = Sunny | Rainy | Stormy deriving (Show, Eq)

data PlayStatus = PlayStatus
                    { hunger  :: Int   -- range [0, 100]
                    , thirsty :: Int   -- range [0, 100]
                    , health  :: Int   -- range [0, 100]
                    , weather :: Weather
                    , date :: Int      -- range [1, 10]
                    } deriving (Show, Eq)

data Option = Option 
                {
                      op1 :: String 
                    , op2 :: String
                    , op3 :: String
                    , op4 :: String

                }

demoOption :: Option      
demoOption = Option { op1 = "Foraging" 
             , op2 = "Hunting"
             , op3 = "Sleeping"
             , op4 = "Collecting water"}