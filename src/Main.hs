module Main where

import Model.Model

main :: IO ()
main = putStrLn $ show $ run [] $ parse $ words "5 6 + 7 8 + *"
