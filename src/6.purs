module Day6 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (fold, fromFoldable, modifyAt, range, updateAt)
import Data.Either (Either)
import Data.Foldable (length)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List, foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set, empty, insert, member)
import Data.String (trim)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Day2 (parseIntChars, readDigit, readInt)
import Debug.Trace (spy)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (sepBy1)
import Text.Parsing.StringParser.String (char)

type Input = Array Int

parseInt :: Parser Int
parseInt = map readInt parseIntChars

line :: Parser Input
line = fromFoldable <$> sepBy1 parseInt (char '\t')

parseString :: String -> Either ParseError Input
parseString input = runParser line input

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

-----

type Index = Int

findLeftMostMaxIndex :: Array Int -> Tuple Int Index
findLeftMostMaxIndex xs = foldlWithIndex folder (0 /\ 0) xs
  where folder i (max /\ maxIndex) x = if x > max then (x /\ i) else (max /\ maxIndex)

inc = (+) 1
modifyAt' i x xs = fromMaybe xs (modifyAt i x xs)

distribute :: Array Int -> Array Int
distribute xs = foldl (\zs i -> modifyAt' (mod (i + maxIndex) (length xs)) inc zs) ys (range 1 maxValue)
  where maxValue /\ maxIndex = findLeftMostMaxIndex xs
        ys = fromMaybe xs $ updateAt maxIndex 0 xs

type State = {
  currentDistribution :: Array Int,
  seen :: Set (Array Int),
  depth :: Int
}

solve :: State -> Int
solve s = if member nextDistribution s.seen then s.depth + 1 else solve $ s {
    currentDistribution = (spy nextDistribution),
    seen = insert nextDistribution s.seen,
    depth = s.depth + 1
  }
  where nextDistribution = distribute s.currentDistribution

solveFromInput input = solve state
  where state = {currentDistribution: input, seen: empty, depth: 0}

solve' = (map <<< map) solveFromInput (getInput "./src/6.txt") >>= logShow

----------

type State2 = {
  currentDistribution :: Array Int,
  seen :: Map.Map (Array Int) Int,
  depth :: Int
}

solve2 :: State2 -> Int
solve2 s = case Map.lookup nextDistribution s.seen of
  Just n -> s.depth - n
  Nothing -> solve2 $ s {
    currentDistribution = (spy nextDistribution),
    seen = Map.insert nextDistribution s.depth s.seen,
    depth = s.depth + 1
  }
  where nextDistribution = distribute s.currentDistribution

solveFromInput2 input = solve2 state
  where state = {currentDistribution: input, seen: Map.empty, depth: 0}

solve2' = (map <<< map) solveFromInput2 (getInput "./src/6.txt") >>= logShow
