module Main where

import Game

main :: IO ()
main = getStdGen >>= playGame . gameSettings
