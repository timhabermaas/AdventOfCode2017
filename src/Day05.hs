module Day05 where

import Data.Vector

f1 :: String -> Int
f1 input = f1' (fromList $ fmap read $ lines input) 0

f1' :: Vector Int -> Int -> Int
f1' instructions pos =
    case instructions !? pos of
        Just jump -> 1 + f1' (instructions // [(pos, jump + 1)]) (pos + jump)
        Nothing -> 0

f2 :: String -> Int
f2 input = f2' (fromList $ fmap read $ lines input) 0

f2' :: Vector Int -> Int -> Int
f2' instructions pos =
    case instructions !? pos of
        Just jump -> 1 + f2' (updatedInstructions jump) (pos + jump)
        Nothing -> 0
  where
    updatedInstructions jump
        | jump >= 3 = instructions // [(pos, jump - 1)]
        | otherwise = instructions // [(pos, jump + 1)]
