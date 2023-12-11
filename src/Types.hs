module Types (module Types) where
import Brick      
import Data.Map as M
import Activity
import Graphics.Vty

-- Define the weather data type
data Weather = Sunny | Rainy | Cloudy deriving (Show, Eq)

type Res = Int

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

fixArt = Art {sunny = "       \\ \\ | / /       \n" ++
"         .-'-.         \n" ++
"  _ --_ /     \\ _--_ __ \n" ++
"=_=-_= -=======- = =-=_\n" ++
" _=-= _--=====- _=-=_-_\n",
rainy = "  __   _\n" ++
    "_(  )_( )_\n" ++
   "(_   _    _)\n" ++
  "/ /(_) (__)\n" ++
 "/ / / / / /\n" ++
"/ / / / / /",
cloudy = "   __   _\n" ++
" _(  )_( )_\n" ++
"(_   _    _)\n" ++
"  (_) (__)\n"}


-- Colored ascii art widgets
data ArtWidget = ArtW {
                    sunnyW :: Widget ()
                  , rainyW :: Widget ()
                  , cloudyW :: Widget ()
                }
-- fixArtWidget = ArtW {sunnyW = withAttr attrBlue $ withBorderStyle unicode $ str "                   \\       /" <|> withAttr attrGreen $ str "            _\\/_\n" <=>
--                      str ".-'-.              //o\\  _\\/_\n" <=>
--  str "_  ___  __  _ --_ /     \\ _--_ __  __ _ | __/o\\\\ _\n" <=>
-- str "=-=-_=-=-_=-=_=-_= -=======- = =-=_=-=_,-'|\"'\"\"-|-,_\n" <=>
-- str "=- _=-=-_=- _=-= _--=====- _=-=_-_,-\"          |",
-- rainyW = withAttr attrWhite $ str "      __   _\n" <=>
--     withAttr attrWhite $ str "_(  )_( )_\n" <=>
--     withAttr attrWhite $ str "(_   _    _)\n" <=>
--     withAttr attrBlue $ str "/ /" <|> withAttr attrWhite $ str "(_) (__)\n" <=>
--     withAttr attrBlue $ str "/ / / / / /\n" <=>
--     withAttr attrBlue $ str "/ / / / / /",
-- cloudyW = withAttr attrWhite $ str "   __   _\n" <=>
--     withAttr attrWhite $ str " _(  )_( )_\n" <=>
--     withAttr attrWhite $ str "(_   _    _)\n" <=>
--     withAttr attrWhite $ str "  (_) (__)\n"}



-- Colors
oliveGreen :: Color
oliveGreen = Graphics.Vty.linearColor 48 51 46
cerulean :: Color
cerulean = Graphics.Vty.linearColor 100 184 213
yellowGreen :: Color
yellowGreen = Graphics.Vty.linearColor 151 219 79
offWhite :: Color
offWhite = Graphics.Vty.linearColor 224 242 233
lilac :: Color
lilac = Graphics.Vty.linearColor 214 163 225

attrBlue :: AttrName
attrBlue = attrName "blue text"
attrGreen :: AttrName
attrGreen = attrName "green text"
attrRed :: AttrName
attrRed = attrName "red text"
attrWhite :: AttrName
attrWhite = attrName "white text"
attrLilac :: AttrName
attrLilac = attrName "lilac text"
