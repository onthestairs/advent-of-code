module Day14 where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (concat, concatMap, cons, filter, mapWithIndex, range)
import Data.Foldable (class Foldable, foldlDefault, length)
import Data.Int (binary, fromStringAs, hexadecimal, toStringAs)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (singleton, toCharArray)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Day10 (knotHash, padLeft)
import Debug.Trace (spy)

key = "ugkiagan"
data Bit = One | Zero
derive instance eqBit :: Eq Bit

makeChars :: Int -> Array Char
makeChars n = toCharArray $ key <> "-" <> show n


charToBits :: Char -> Array Bit
charToBits c = map (\c -> if c == '1' then One else Zero) $ toCharArray $ padLeft '0' 4 $ toStringAs binary $ fromMaybe 0 $ fromStringAs hexadecimal (singleton c)

hashToBits :: String -> Array Bit
hashToBits s = concatMap charToBits (toCharArray s)

-- solve :: Int
-- solve = length $ filter ((==) One) (concat hashBits)
--   where n = 128
--         hashes = map (knotHash <<< makeChars) (range 0 (n-1))
--         hashBits = map hashToBits hashes

-----

type Coord = Tuple Int Int
type OnBits = Set.Set (Coord)
--

makeOnBits :: Int -> OnBits
makeOnBits n = Set.fromFoldable $ map fst $ filter (\(Tuple _ bit) -> bit == One) $ concat $ mapWithIndex (\row bits -> mapWithIndex (\col bit -> Tuple (row /\ col) bit) bits) hashBits
  where hashes = map (knotHash <<< makeChars) (range 0 (n-1))
        hashBits = map hashToBits hashes

adjacents :: Coord -> List.List Coord
adjacents (row /\ col) = List.fromFoldable [
  (row + 1) /\ col,
  (row - 1) /\ col,
  row /\ (col + 1),
  row /\ (col - 1)
]

popSet :: forall a. Ord a => Set.Set a -> Maybe {value :: a, rest :: Set.Set a}
popSet s = case Set.findMin s of
  Nothing -> Nothing
  Just x -> Just {value: x, rest: Set.delete x s}

connectedBits :: OnBits -> Coord -> List.List Coord
connectedBits onBits c = tailRec go {connected: List.Nil, seen: List.Nil, queue: List.singleton c}
  where go state = case List.uncons state.queue of
                    Nothing -> Done state.connected
                    Just {head: x, tail: xs} ->
                      if Set.member x onBits
                      then Loop {
                        connected: List.Cons x state.connected,
                        seen: List.Cons x state.seen,
                        queue: List.nub $ xs <> (List.difference (adjacents x) (state.seen))
                      }
                      else Loop {
                        connected: state.connected,
                        seen: List.Cons x state.seen,
                        queue: xs
                      }

connectedRegions :: forall k f. Ord k => Foldable f => (f k) -> (k -> List.List k) -> Int
connectedRegions ns f = length $ foldlDefault (\ms n -> go n ms) List.Nil ns
  where go n ms = if (List.elem n (List.concat ms)) then ms else (List.Cons (f n) ms)

solve2 n = connectedRegions onBits (connectedBits onBits)
  where onBits = makeOnBits n

-- main = solve2 10

-- testOnBits = Set.fromFoldable [(0 /\ 0), (0 /\ 1), (2 /\ 0)]
--
-- solveTest = connectedRegions onBits (connectedBits onBits)
--   where onBits = testOnBits
--
--
-- solveTest' = connectedBits onBits (0 /\ 0)
--   where onBits = testOnBits
