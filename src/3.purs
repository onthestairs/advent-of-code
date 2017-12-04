module Day3 where

import Prelude

import Data.Array (catMaybes, foldl, length, range, replicate, scanl, (!!))
import Data.Foldable (sum)
import Data.Int (ceil, even, floor, toNumber)
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Ord (abs)
import Data.StrMap (StrMap, empty, insert, lookup, singleton)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Math (sqrt)

type Coord = Tuple Int Int

addCoord :: Coord -> Coord -> Coord
addCoord (r1 /\ c1) (r2 /\ c2) = (r1 + r2) /\ (c1 + c2)
infixr 6 addCoord as /+\

intToRing :: Int -> Int
intToRing n = ceil $ ((sqrt (toNumber n) - 1.0) / 2.0)

makeDeltas ring = right <> top <> left <> bottom
  where edgeSize = ringToEdgeSize ring
        right = replicate (edgeSize - 2) (-1 /\ 0)
        top = replicate (edgeSize - 1) (0 /\ -1)
        left = replicate (edgeSize - 1) (1 /\ 0)
        bottom = replicate (edgeSize - 1) (0 /\ 1)

ringToEdgeSize n = (n * 2) + 1
ringToStartCoord n = (0 /\ 0) /+\ (n-1 /\ 0) /+\ (0 /\ n)
ringStartNumber n = (2*n-1)*(2*n-1)+1

intToCoord :: Int -> Maybe Coord
intToCoord n = (/+\) startCoord <$> (deltasFromStart !! offset)
  where
    ring = intToRing n
    startCoord = ringToStartCoord ring
    deltasFromStart = [(0 /\0)] <> scanl (/+\) (0 /\ 0) (makeDeltas ring)
    offset = n - ringStartNumber ring

manhattanDistance :: Coord -> Int
manhattanDistance (row /\ col) = abs row + abs col

solve = manhattanDistance <$> intToCoord 347991

---------

type CoordToValue = StrMap Int

adjacentCoords :: Coord -> Array Coord
adjacentCoords c = map ((/+\) c) deltas
  where deltas = [1 /\ 0, 1 /\ 1, 1 /\ -1, 0 /\ 1, 0 /\ -1, -1 /\ 0, -1 /\ 1, -1 /\ -1]

getValueForN :: CoordToValue -> Int -> (Tuple Coord Int)
getValueForN m n = coord /\ v
  where coord = fromMaybe (0 /\ 0) $ intToCoord n
        coords = adjacentCoords coord
        v = sum $ catMaybes $ map (\c -> lookup (show c) m) coords

solve2 :: CoordToValue -> Int -> Int -> Int
solve2 m n target = if value > target then value else solve2 (insert (show coord) value m) (n+1) target
  where (coord /\ value) = getValueForN m n

solve2' = solve2 (singleton (show (0 /\ 0)) 1) 2 347991
