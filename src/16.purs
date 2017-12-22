module Day16 where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Fold (Fold, foldl, unfoldFold_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (length)
import Data.List (List, drop, findIndex, fromFoldable, take, updateAt, (!!))
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (power)
import Data.String (fromCharArray, joinWith, trim)
import Day2 (parseInt)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (sepBy1)
import Text.Parsing.StringParser.String (anyLetter, char)

data DanceMove = Spin Int | Exchange Int Int | Partner Char Char
type Input = List DanceMove

parseSpin :: Parser DanceMove
parseSpin = do
  _ <- char 's'
  n <- parseInt
  pure (Spin n)

parseExchange :: Parser DanceMove
parseExchange = do
  _ <- char 'x'
  a <- parseInt
  _ <- char '/'
  b <- parseInt
  pure (Exchange a b)

parsePartner :: Parser DanceMove
parsePartner = do
  _ <- char 'p'
  a <- anyLetter
  _ <- char '/'
  b <- anyLetter
  pure (Partner a b)

parseDanceMove :: Parser DanceMove
parseDanceMove =
  parseSpin <|>
  parseExchange <|>
  parsePartner

parseString :: String -> Either ParseError Input
parseString = runParser $ sepBy1 parseDanceMove (char ',')

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)


type Line = Array Char
startingLine :: Line

startingLine = Array.fromFoldable $ ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p']

dance :: Line -> Fold DanceMove Line
dance l = unfoldFold_ l performDanceMove

performDanceMove :: Line -> DanceMove -> Line
performDanceMove l (Spin n) = spin l n
performDanceMove l (Exchange a b) = exchange l a b
performDanceMove l (Partner a b) = partner l a b

spin :: Line -> Int -> Line
spin l n = Array.drop m l <> Array.take m l
  where m = (length l) - n

exchange :: Line -> Int -> Int -> Line
exchange l a b = fromMaybe l $ do
  aVal <- l Array.!! a
  bVal <- l Array.!! b
  aReplaced <- Array.updateAt a bVal l
  Array.updateAt b aVal aReplaced

partner :: Line -> Char -> Char -> Line
partner l a b = fromMaybe l $ lift2 (exchange l) aIndex bIndex
  where aIndex = Array.findIndex ((==) a) l
        bIndex = Array.findIndex ((==) b) l


solve danceMoves = fromCharArray $ finalPosition
  where finalPosition = foldl (dance startingLine) danceMoves
solve' = (map <<< map) solve (getInput "./src/16.txt") >>= logShow


---------

repeatF :: forall a. (a -> a) -> Int -> (a -> a)
repeatF f 0 = id
repeatF f n = f <<< (repeatF f (n-1))

oneBillion = 1 * 1000 * 1000


solve2 :: Input -> String
solve2 danceMoves = fromCharArray $ (repeatF fullDance fromStart) startingLine
  where fullDance l = foldl (dance l) danceMoves
        cycleLength = tailRec go {position: fullDance startingLine, n: 1}
                      where go state = if state.position == startingLine then Done state.n else Loop {position: fullDance state.position, n: state.n + 1}
        fromStart = oneBillion `mod` cycleLength

solve2' = (map <<< map) solve2 (getInput "./src/16.txt") >>= logShow
