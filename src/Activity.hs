module Activity where

import qualified Game as G

actFish       = G.Activity "Fish"           2 5  0 (-5) 0.5
actPick       = G.Activity "Pick Coconuts"  2 5 10 (-5) 0.5
actDrink      = G.Activity "Drink water"    1 0 10 (-5) 0.5
actNothing    = G.Activity "Do nothing"     1 0  0    1 0.5
