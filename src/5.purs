module Day5 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (fromFoldable, modifyAt, (!!))
import Data.Either (Either)
import Data.Foldable (length)
import Data.List (List, foldl)
import Data.Maybe (Maybe, fromMaybe, isJust, maybe)
import Data.String (trim)
import Day2 (parseIntChars, readDigit)
import Debug.Trace (spy)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (optionMaybe, sepBy1)
import Text.Parsing.StringParser.String (char)

type Input = Array Int

readInt :: (List Char) -> Int
readInt xs = foldl (\acc n -> (acc * 10) + n) 0 (map readDigit xs)

parseInt :: Parser Int
parseInt = map readInt parseIntChars

line :: Parser Int
line = do
  neg <- optionMaybe (char '-')
  let multiplier = maybe 1 (const (-1)) neg
  int <- parseInt
  pure (multiplier * int)

lines :: Parser Input
lines = fromFoldable <$> sepBy1 line (char '\n')

parseString :: String -> Either ParseError Input
parseString input = runParser lines input

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

---


incAt :: Array Int -> Int -> Maybe (Array Int)
incAt xs n = modifyAt n ((+) 1) xs

solve :: Int -> Int -> Input -> Int
solve index depth xs = if newIndex < 0 || newIndex >= upperBound then (depth + 1) else solve (newIndex) (depth+1) (fromMaybe xs $ incAt xs index)
  where newIndex = index + (fromMaybe 0 $ xs !! index)
        upperBound = length xs

solve' = (map <<< map) (solve 0 0) (getInput "./src/5.txt") >>= logShow


----

incAt2 :: Array Int -> Int -> Maybe (Array Int)
incAt2 xs n = modifyAt n f xs
  where f n = if n >= 3 then n-1 else n+1

solve2 :: Int -> Int -> Input -> Int
solve2 index depth xs = if newIndex < 0 || newIndex >= upperBound then (depth + 1) else solve2 (newIndex) (depth+1) (fromMaybe xs $ incAt2 xs index)
  where newIndex = index + (fromMaybe 0 $ xs !! index)
        upperBound = length xs

solve2' = (map <<< map) (solve2 0 0) (getInput "./src/5.txt") >>= logShow
