module Day9 where

import Prelude

import Control.Alt ((<|>))
import Control.Biapplicative (bipure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Foldable (sum)
import Data.Generic (class Generic, gShow)
import Data.List (List, concat, foldl)
import Data.String (fromCharArray, length, singleton, trim)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (between, fix, many, many1, sepBy)
import Text.Parsing.StringParser.String (anyChar, char, noneOf)

data Block = Garbage String | Group (List Block)
derive instance genericBlock :: Generic Block
instance showBlock :: Show Block
  where show = gShow

parseBlock :: Parser Block -> Parser Block
parseBlock p = parseGroup p <|> parseGarbage

parseGroup :: Parser Block -> Parser Block
parseGroup p = Group <$> between (char '{') (char '}') (sepBy p (char ','))

concatStrings :: List String -> String
concatStrings xs = foldl (\a x -> append a x) "" xs

parseGarbage :: Parser Block
parseGarbage = Garbage <$> between (char '<') (char '>') garbageInner
  where garbageInner = concatStrings <$> (many $ (escapedChar <|> legalChar))
        escapedChar = do
          _ <- char '!'
          _ <- anyChar
          pure ""
        legalChar = singleton <$> noneOf ['>']

parseString :: String -> Either ParseError Block
parseString = runParser $ fix $ \p -> parseBlock p


getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Block)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)


-----------

solve :: Block -> Int
solve b = go 1 b
  where go depth (Garbage _) = 0
        go depth (Group bs) = depth + sum (map (go (depth + 1)) bs)

solve' = (map <<< map) solve (getInput "./src/9.txt") >>= logShow

------

solve2 :: Block -> Int
solve2 (Garbage g) = (length g)
solve2 (Group bs) = sum $ map solve2 bs

solve2' = (map <<< map) solve2 (getInput "./src/9.txt") >>= logShow
