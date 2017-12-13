module Day13 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (maximum, sum)
import Data.Generic (class Generic, gShow, showDataConstructor)
import Data.List as List
import Data.List (List, filter, range, scanl, zip, (:), any)
import Data.List.Lazy as LazyList
import Data.List.NonEmpty (zipWithA)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (curry1, uncurry1, (/\))
import Day8 (parseSignedInt)
import Debug.Trace (spy)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (sepBy1)
import Text.Parsing.StringParser.String (char, string)

type Depth = Int
type Range = Int
type Input = List (Tuple Depth Range)

parseLine :: Parser (Tuple Depth Range)
parseLine = do
  depth <- parseSignedInt
  _ <- string ": "
  range <- parseSignedInt
  pure (Tuple depth range)

parseString :: String -> Either ParseError Input
parseString = runParser $ sepBy1 parseLine (char '\n')

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

-----

type Offset = Int
data CaughtStatus = Caught Int | NotCaught

isCaught :: CaughtStatus -> Boolean
isCaught (Caught _) = true
isCaught _ = false

caughtSeverity :: CaughtStatus -> Int
caughtSeverity (Caught n) = n
caughtSeverity _ = 0

getCaughtStatuses :: Input -> Offset -> List CaughtStatus
getCaughtStatuses input offset = map getSeverity input
  where getSeverity (d /\ r) = if (d+offset) `mod` (2*r - 2) == 0
                               then Caught (d*r)
                               else NotCaught


getSeverity :: Input -> Int -> Int
getSeverity input offset = sum $ Array.fromFoldable $ map caughtSeverity $ filter isCaught $ getCaughtStatuses input offset

solve :: Input -> Int
solve input = getSeverity input 0

solve' = (map <<< map) solve (getInput "./src/13.txt") >>= logShow

----

wasCaught :: Input -> Offset -> Boolean
wasCaught input offset = any isCaught $ getCaughtStatuses input offset

solve2 :: Input -> Int
solve2 input = List.length $ List.takeWhile (\n -> wasCaught input n) offsets
  where offsets = List.range 0 10000000

solve2' = (map <<< map) solve2 (getInput "./src/13.txt") >>= logShow
