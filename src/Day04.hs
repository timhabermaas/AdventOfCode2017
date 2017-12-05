module Day04 where

import Data.Set
import Data.List (sort)

f1 :: String -> Int
f1 text = length $ Prelude.filter valid $ (words <$> lines text)
  where
    valid words = length words == (length $ fromList words)

f2 :: String -> Int
f2 text = length $ Prelude.filter valid $ (words <$> lines text)
  where
    valid words = length words == (length $ unique $ normalize <$> words)
    unique :: [String] -> Set String
    unique = fromList
    normalize = sort
