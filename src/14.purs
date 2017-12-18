module Day14 where

import Prelude

import Data.Array (concat, concatMap, filter, range, mapWithIndex)
import Data.Foldable (length)
import Data.Graph (unfoldGraph)
import Data.Graph as Graph
import Data.Int (binary, fromStringAs, hexadecimal, toStringAs)
import Data.List (List(..), catMaybes, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap as Map
import Data.String (Pattern(..), singleton, split, toCharArray)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Day10 (knotHash)
import Day12 (connectedRegions)
import Day5 (readInt)
import Debug.Trace (spy)
import Partial.Unsafe (unsafePartial)

key = "ugkiagan"
data Bit = One | Zero
derive instance eqBit :: Eq Bit

makeChars :: Int -> Array Char
makeChars n = toCharArray $ key <> "-" <> show n

charToBits :: Char -> Array Bit
charToBits c = map (\c -> if c == '1' then One else Zero) $ toCharArray $ toStringAs binary $ fromMaybe 0 $ fromStringAs hexadecimal (singleton c)

hashToBits :: String -> Array Bit
hashToBits s = concatMap charToBits (toCharArray s)

solve :: Int
solve = length $ filter ((==) One) (concat hashBits)
  where n = 128
        hashes = map (knotHash <<< makeChars) (range 0 (n-1))
        hashBits = map hashToBits hashes

-----

type Coord = Tuple Int Int
s :: Int -> Int -> String
s row col = show row <> "," <> show col
type Bits = Map.StrMap Bit

bits :: Bits
bits = Map.fromFoldable $ concat $ mapWithIndex (\row bits -> mapWithIndex (\col bit -> Tuple (s row col) bit) bits) hashBits
  where n = 128
        hashes = map (knotHash <<< makeChars) (range 0 (n-1))
        hashBits = map hashToBits hashes

-- findAdjacents :: Coord -> Array Coord
-- findAdjacents (row /\ col) = [
--   (row + 1) /\ col,
--   (row - 1) /\ col,
--   row /\ (col + 1),
--   row /\ (col - 1)
-- ]
findAdjacents :: Partial => String -> Array String
findAdjacents t = map (\(row /\ col) -> s row col) [
  (row + 1) /\ col,
  (row - 1) /\ col,
  row /\ (col + 1),
  row /\ (col - 1)
]
  where [rowS, colS] = split (Pattern ",") t
        row = (readInt <<< List.fromFoldable <<< toCharArray) rowS
        col = (readInt <<< List.fromFoldable <<< toCharArray) colS

makeGraph :: Bits -> Graph.Graph String String
makeGraph bits = unfoldGraph oneCoords id adjacentOneCoords
  where oneCoords = Map.keys $ Map.filter ((==) One) bits
        adjacentOneCoords :: String -> Array String
        adjacentOneCoords c = filter isOne (unsafePartial $ findAdjacents c)
        isOne :: String -> Boolean
        isOne t = (Map.lookup t bits) == (Just One)

solve2 = connectedRegions g
  where g = makeGraph bits
        -- blah = spy ((length $ Graph.vertices g) :: Int)
