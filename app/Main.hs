module Main where

import Day04

main :: IO ()
main = do
    input <- readFile "inputs/Day04.txt"
    putStrLn $ show $ f1 $ input
    putStrLn $ show $ f2 $ input
