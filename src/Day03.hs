module Day03 where

import Data.Monoid
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import Control.Monad (guard)
import Debug.Trace (traceShow, trace)

-- take number, determine minimum grid size (y x y)
-- take number, determine coordinate (relative to center?)

-- 25
-- circle: [(n-1) ^ 2 + 1, n ^ 2
-- count of numbers outer ring: bottom: n, left + right: (n - 1), top: n => 2n + 2(n - 1) = 4n - 2

distance :: (Int, Int) -> (Int, Int) -> Int
distance (a1, b1) (a2, b2) = abs (a1 - a2) + abs (b1 - b2)

f1 :: Int -> Int
f1 x = distance (getCoordinate x) (0, 0)
  where
    n = minimumGridSize x

-- This could be solved by using sqrt + rounding...
minimumGridSize :: Int -> Int
minimumGridSize x = head [y | y <- [1..], y*y >= x]

getCoordinate :: Int -> (Int, Int)
getCoordinate x
    | x `elem` [n * n - n + 1..n * n] = (x - (n * n - n + 1) - n `div` 2, n - 1 - (if n `mod` 2 == 0 then n `div` 2 - 1 else n `div` 2))
    | x `elem` [n * n - n - n + 2..n * n - n] = (0, x - (n * n - n - n + 2))
  where
    n = minimumGridSize x


data Move = TurnLeft | MoveForward deriving Show

newtype Vector = Vector { getVec :: (Int, Int) } deriving (Show, Ord, Eq)

instance Monoid Vector where
  mempty = Vector (0, 0)
  mappend (Vector (x1, y1)) (Vector (x2, y2)) = Vector (x1 + x2, y1 + y2)


f2 :: Int -> Int
f2 max = evalState (f2' max $ tail (coordinatesFromMove spiral)) (Map.singleton (Vector (0, 0)) 1)
               -- If we keep rotating the grid we might get away with a simple rule how the get the adjacent grid numbers
               --

f2' :: Int -> [Vector] -> State (Map.Map Vector Int) Int
f2' max (pos:ps) = do
    map <- get
    let neighbours' = neighbours pos
    let neighbourSum = sum $ catMaybes $ fmap (\n -> Map.lookup n map) neighbours'
    put $ Map.insert pos neighbourSum map
    if neighbourSum > max then
        return neighbourSum
    else
        f2' max ps

neighbours :: Vector -> [Vector]
neighbours v = [Vector (x, y) <> v | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

spiral :: [Move]
spiral = spiral' 1
  where
    spiral' n = replicate MoveForward n (TurnLeft : replicate MoveForward n (TurnLeft : spiral' (n + 1)))
    replicate x n rest
      | n == 0    = rest
      | otherwise = x : replicate x (n - 1) rest

rotateLeft :: Vector -> Vector
rotateLeft (Vector (1, 0)) = Vector (0, -1)
rotateLeft (Vector (0, 1)) = Vector (1, 0)
rotateLeft (Vector (-1, 0)) = Vector (0, 1)
rotateLeft (Vector (0, -1)) = Vector (-1, 0)

-- TODO: Replace with fold
coordinatesFromMove :: [Move] -> [Vector]
coordinatesFromMove moves = coordinatesFromMove' (Vector (1, 0)) moves mempty
  where
    coordinatesFromMove' dir [] pos = [pos]
    coordinatesFromMove' dir (MoveForward:ms) pos = pos : coordinatesFromMove' dir ms (pos <> dir)
    coordinatesFromMove' dir (TurnLeft:ms) pos = coordinatesFromMove' (rotateLeft dir) ms pos


nextCoordinate :: (Int, Int) -> (Int, Int)
nextCoordinate (x, y) = (0, 0)
