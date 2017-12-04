module Day4 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (fromFoldable, sort)
import Data.Either (Either)
import Data.Foldable (length)
import Data.List (List, filter, nub)
import Data.String (fromCharArray, toCharArray, trim)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (many, sepBy1)
import Text.Parsing.StringParser.String (char, lowerCaseChar, regex)

type Input = List (List String)

line :: Parser (List String)
line = sepBy1 ((fromCharArray <<< fromFoldable) <$> many lowerCaseChar) (char ' ')

lines :: Parser Input
lines = sepBy1 line (char '\n')

parseString :: String -> Either ParseError Input
parseString input = runParser lines input

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

----

isValid :: List String -> Boolean
isValid xs = ((length xs) :: Int) == (length $ nub xs)

solve :: Input -> Int
solve xss = length $ filter isValid xss

solve' = (map <<< map) solve (getInput "./src/4.txt") >>= logShow

-----

isValid2 :: List String -> Boolean
isValid2 xs = ((length ys) :: Int) == (length $ nub ys)
  where ys = map (fromCharArray <<< sort <<< toCharArray) xs

solve2 :: Input -> Int
solve2 xss = length $ filter isValid2 xss

solve2' = (map <<< map) solve2 (getInput "./src/4.txt") >>= logShow
