module Main where

import Day02

main :: IO ()
main = do
    input <- readFile "inputs/Day02.txt"
    putStrLn $ show $ f1 $ input
    putStrLn $ show $ f2 $ input
