module Day02 where

import Data.List.Split (splitOneOf)

f1 :: String -> Int
f1 = checksumPerRow maxMinDifference
  where
    maxMinDifference line = maximum line - minimum line

f2 :: String -> Int
f2 = checksumPerRow quotientEvenlyDivisible
  where
    quotientEvenlyDivisible line = head [a `div` b | a <- line, b <- line, a `mod` b == 0, a > b]

checksumPerRow :: ([Int] -> Int) -> String -> Int
checksumPerRow f input = sum $ f . fmap read . splitOneOf " \t" <$> lines input
