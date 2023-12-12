module Types (module Types) where
import Brick      
import Data.Map as M
import Activity
import Graphics.Vty
import Brick.Widgets.Center

data Choice = W | A | S | D | None deriving (Show, Eq)

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
                    , choice :: Choice
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
fixArtWidget = ArtW { sunnyW = vBox [hCenter $ str $ " ", 
                                withAttr attrRed $ str $ "       \\ \\ | / /       \n",
                                withAttr attrRed $ str $ "         .-'-.         \n",
                                hBox [withAttr attrBlue $ str $ "  _ --_ ", withAttr attrRed $ str $ "/     \\" ,withAttr attrBlue $ str $ " _--_ __ \n"],
                                withAttr attrBlue $ str $ "=_=-_= -=======- = =-=_\n",
                                withAttr attrBlue $ str $ " _=-= _--=====- _=-=_-_\n",
                                hCenter $ str $ " " ],
                      rainyW = vBox [withAttr attrWhite $ str "       __   _\n",
                          withAttr attrWhite $ str "     _(  )_( )_\n",
                          withAttr attrWhite $ str "    (_   _    _)\n",
                          hBox [withAttr attrBlue $ str "    / ", withAttr attrWhite $ str "(_) (__)\n"],
                          withAttr attrBlue $ str "    / / / / / /\n",
                          withAttr attrBlue $ str "    / / / / / /",
                          hCenter $ str $ " "],
                      cloudyW = vBox [withAttr attrWhite $ str "       __   _\n",
                          withAttr attrWhite $ str "     _(  )_( )_\n",
                          withAttr attrWhite $ str "    (_   _    _)\n",
                          withAttr attrWhite $ str "      (_) (__)\n",
                          hCenter $ str $ " ", 
                          hCenter $ str $ " ", 
                          hCenter $ str $ " "]
}

islandWidget :: Widget ()
islandWidget = vBox [ withAttr attrGreen $ str "        _ ^ _",
                      withAttr attrGreen $ str "       '_\\V/ `",
                      hBox [withAttr attrGreen $ str "       '  ", withAttr attrLilac $ str "X", withAttr attrGreen $ str "`"],
                      withAttr attrLilac $ str "          X",
                      withAttr attrLilac $ str "          X             -HELP!",
                      withAttr attrLilac $ str "          X",
                      hBox [withAttr attrLilac $ str "          X        ", withAttr attrRed $ str "\\O/"],
                      hBox [withAttr attrLilac $ str "          X", withAttr attrGreen $ str ".a##a.   ", withAttr attrRed $ str "M"],
                      hBox [withAttr attrGreen $ str "       .aa########a.", withAttr attrRed $ str ">>"],
                      withAttr attrGreen $ str "    .a################aa.",
                      withAttr attrBlue $ str "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
                      hCenter $ str $ " "]


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
attrBold :: AttrName
attrBold = attrName "bold lilac text"
attrBoldW :: AttrName
attrBoldW = attrName "bold white text"