module Day06 where

import qualified Data.Set as Set
import Data.List (transpose)
import Debug.Trace (traceShow, traceShowId)

f1 :: String -> Int
f1 input = snd $ f1' (read <$> words input) Set.empty

f2 :: String -> Int
f2 input =
    let
      (seenState, _) = f1' (read <$> words input) Set.empty
    in
      (\x -> x - 1) $ snd $ f1' seenState Set.empty


f1' :: [Int] -> Set.Set [Int] -> ([Int], Int)
f1' startState seenStates =
    let
      (maxElement, maxIndex) = maximumWithIndex startState
      drops = replicate (maxIndex + 1) 0 ++ replicate maxElement 1
      shrunkDrops = fmap sum $ transpose $ partitionWithNullPadding (length startState) drops
      newState = sumList shrunkDrops $ nullIndex maxIndex startState
    in
      if Set.member newState seenStates then
          (newState, 1)
      else
          (+ 1) <$> f1' newState (Set.insert newState seenStates)

nullIndex :: Num a => Int -> [a] -> [a]
nullIndex 0 (x:xs) = 0:xs
nullIndex i (x:xs) = x:nullIndex (i - 1) xs

sumList :: Num a => [a] -> [a] -> [a]
sumList = zipWith (+)

partitionWithNullPadding :: Num a => Int -> [a] -> [[a]]
partitionWithNullPadding l [] = []
partitionWithNullPadding l xs
    | length xs <= l = [xs ++ replicate (l - length xs) 0]
    | otherwise      = take l xs:partitionWithNullPadding l (drop l xs)

maximumWithIndex :: Ord a => [a] -> (a, Int)
-- Flipping index list in order to prioritize elements at the beginning.
maximumWithIndex xs = fmap (* (-1)) $ maximum $ zip xs $ fmap (* (-1)) [0..(length xs - 1)]
