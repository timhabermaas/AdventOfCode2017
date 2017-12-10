module Main where

import Day06

main :: IO ()
main = do
    input <- readFile "inputs/Day06.txt"
    putStrLn $ show $ f1 input
    putStrLn $ show $ f2 input
