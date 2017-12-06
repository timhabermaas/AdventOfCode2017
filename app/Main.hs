module Main where

import Day03

main :: IO ()
main = do
    input <- readFile "inputs/Day04.txt"
    putStrLn $ show $ f1 289326
    putStrLn $ show $ f2 289326
