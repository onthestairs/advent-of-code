module Day2 where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Date (year)
import Data.Either (Either)
import Data.Foldable (maximum, minimum, sum)
import Data.List (List, catMaybes, foldl)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import Data.String (trim)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.Parser.Token (digit)
import Text.Parsing.StringParser (ParseError(..), Parser, runParser)
import Text.Parsing.StringParser.Combinators (many1, sepBy1)
import Text.Parsing.StringParser.String (anyDigit, char)

type Input = List (List Int)

parseIntChars :: Parser (List Char)
parseIntChars = many1 anyDigit

readDigit :: Char -> Int
readDigit '0' = 0
readDigit '1' = 1
readDigit '2' = 2
readDigit '3' = 3
readDigit '4' = 4
readDigit '5' = 5
readDigit '6' = 6
readDigit '7' = 7
readDigit '8' = 8
readDigit '9' = 9
readDigit _ = 0 -- how to fix this?

readInt :: (List Char) -> Int
readInt xs = foldl (\acc n -> (acc * 10) + n) 0 (map readDigit xs)

parseInt :: Parser Int
parseInt = map readInt parseIntChars

line :: Parser (List Int)
line = sepBy1 (parseInt) (char '\t')

lines :: Parser Input
lines = sepBy1 line (char '\n')

parseString :: String -> Either ParseError Input
parseString input = runParser lines input

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

----
rowChecksum :: List Int -> Maybe Int
rowChecksum xs = lift2 (-) (maximum xs) (minimum xs)

solve :: Input -> Int
solve xss = sum $ catMaybes $ map rowChecksum xss

solve' = (map <<< map) solve (getInput "./src/2.txt") >>= logShow

----

rowChecksum2 :: List Int -> Int
rowChecksum2 xs = sum $ map (\x -> sum $ map (\y -> if x `mod` y == 0 && x > y then x / y else 0) xs) xs

solve2 :: Input -> Int
solve2 xss = sum $ map rowChecksum2 xss

solve2' = (map <<< map) solve2 (getInput "./src/2.txt") >>= logShow
