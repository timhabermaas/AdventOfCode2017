module Main where

import Day05

main :: IO ()
main = do
    input <- readFile "inputs/Day05.txt"
    putStrLn $ show $ f1 input
    putStrLn $ show $ f2 input
