module Day14 where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (concat, concatMap, cons, elem, filter, mapWithIndex, range, uncons)
import Data.Array as Array
import Data.Foldable (class Foldable, foldlDefault, length)
import Data.Graph (unfoldGraph)
import Data.Graph as Graph
import Data.Int (binary, fromStringAs, hexadecimal, toStringAs)
import Data.List (List(..), catMaybes, (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (Pattern(..), singleton, split, toCharArray)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Day10 (knotHash, padLeft)
import Day5 (readInt)
import Debug.Trace (spy)
import Partial.Unsafe (unsafePartial)

key = "ugkiagan"
data Bit = One | Zero
derive instance eqBit :: Eq Bit

makeChars :: Int -> Array Char
makeChars n = toCharArray $ key <> "-" <> show n


charToBits :: Char -> Array Bit
charToBits c = map (\c -> if c == '1' then One else Zero) $ toCharArray $ padLeft '0' 4 $ toStringAs binary $ fromMaybe 0 $ fromStringAs hexadecimal (singleton c)

hashToBits :: String -> Array Bit
hashToBits s = concatMap charToBits (toCharArray s)

solve :: Int
solve = length $ filter ((==) One) (concat hashBits)
  where n = 128
        hashes = map (knotHash <<< makeChars) (range 0 (n-1))
        hashBits = map hashToBits hashes

-----

type Coord = Tuple Int Int
type OnBits = Set.Set (Coord)
--
makeOnBits :: OnBits
makeOnBits = Set.fromFoldable $ map fst $ filter (\(Tuple _ bit) -> bit == One) $ concat $ mapWithIndex (\row bits -> mapWithIndex (\col bit -> Tuple (row /\ col) bit) bits) hashBits
  where n = 128
        hashes = map (knotHash <<< makeChars) (range 0 (n-1))
        hashBits = map hashToBits hashes

adjacents :: Coord -> Set.Set Coord
adjacents (row /\ col) = Set.fromFoldable [
  (row + 1) /\ col,
  (row - 1) /\ col,
  row /\ (col + 1),
  row /\ (col - 1)
]

popSet :: forall a. Ord a => Set.Set a -> Maybe {value :: a, rest :: Set.Set a}
popSet s = case Set.findMin s of
  Nothing -> Nothing
  Just x -> Just {value: x, rest: Set.delete x s}

connectedBits :: OnBits -> Coord -> Set.Set Coord
connectedBits onBits c = tailRec go {connected: Set.empty, queue: Set.singleton c}
  where go state = case popSet state.queue of
                    Nothing -> Done state.connected
                    Just {value: x, rest: xs} ->
                      if Set.member x onBits
                      then Loop {connected: Set.insert x state.connected, queue: Set.union xs (Set.difference (adjacents x) state.connected)}
                      else Loop {connected: state.connected, queue: xs}

connectedRegions :: forall k f. Ord k => Foldable f => (f k) -> (k -> Set.Set k) -> Int
connectedRegions ns f = length $ foldlDefault (\ms n -> go n ms) [] ns
  where go n ms = if (Set.member n (Set.unions ms)) then ms else (cons (f n) ms)

solve2 = connectedRegions onBits (connectedBits onBits)
  where onBits = makeOnBits

--
-- testOnBits = Set.fromFoldable [(0 /\ 0), (1 /\ 0), (2 /\ 0)]
--
-- solveTest = connectedRegions onBits (connectedBits onBits)
--   where onBits = testOnBits
--
--
-- solveTest' = connectedBits onBits (0 /\ 0)
--   where onBits = testOnBits
