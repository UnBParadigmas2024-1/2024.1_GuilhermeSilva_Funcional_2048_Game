module Main where

import Graphics.Gloss
import UIConfig
import Game (runGame)

main :: IO ()
main = playBanana
    (InWindow
      "2048"
      (480, 640)
      (10, 10))
    black
    60
    runGame
