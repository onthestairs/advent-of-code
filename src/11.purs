module Day11 where

import Prelude

import Control.Alt ((<|>))
import Control.Fold (scanl, unfoldFold_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either)
import Data.Foldable (length, maximum)
import Data.List (List(..), foldl, take)
import Data.Maybe (Maybe)
import Data.Ord (abs)
import Data.String (trim)
import Debug.Trace (spy)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (sepBy1)
import Text.Parsing.StringParser.String (char, string)

------
-- based on https://www.redblobgames.com/grids/hexagons/implementation.html#hex-arithmetic

data Hex = Hex {
  q :: Int,
  r :: Int
}

q :: Hex -> Int
q (Hex {q, r}) = q
r :: Hex -> Int
r (Hex {q, r}) = r
s :: Hex -> Int
s (Hex {q, r}) = 0 - q - r

zero :: Hex
zero = Hex {
  q: 0,
  r: 0
}

add :: Hex -> Hex -> Hex
add h1 h2 = Hex {
  q: (q h1) + (q h2),
  r: (r h1) + (r h2)
}

subtract :: Hex -> Hex -> Hex
subtract h1 h2 = Hex {
  q: (q h1) - (q h2),
  r: (r h1) - (r h2)
}

hlength :: Hex -> Int
hlength h = (abs (q h) + abs (r h) + abs (s h)) / 2

distance :: Hex -> Hex -> Int
distance h1 h2 = hlength (subtract h1 h2)

---------
data Direction = SE | S | SW | NW | N | NE

type Input = List Direction

parseDirection :: Parser Direction
parseDirection =
  (string "se" $> SE) <|>
  (string "sw" $> SW) <|>
  (string "s" $> S) <|>
  (string "ne" $> NE) <|>
  (string "nw" $> NW) <|>
  (string "n" $> N)

parseString :: String -> Either ParseError Input
parseString = runParser $ sepBy1 parseDirection (char ',')

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

directionToDelta :: Direction -> Hex
directionToDelta S  = Hex {q: 0,  r: -1}
directionToDelta SW = Hex {q: -1, r: 0}
directionToDelta SE = Hex {q: 1,  r: -1}
directionToDelta NE = Hex {q: 1,  r: 0}
directionToDelta NW = Hex {q: -1, r: 1}
directionToDelta N  = Hex {q: 0,  r: 1}

solve :: List Direction -> Int
solve ds = hlength $ foldl add zero (map directionToDelta ds)

solve' = (map <<< map) solve (getInput "./src/11.txt") >>= logShow

------

-- custom recursion as stack was getting too large with a scanl. baffled as
-- there are only ~8,000 directions

solve2 :: List Direction -> Int
solve2 ds = go 0 zero ds
  where go max h Nil = max
        go max h (Cons e es) = if l > max then go l f es else go max f es
          where f = add h (directionToDelta e)
                l = hlength f

solve2' = (map <<< map) solve2 (getInput "./src/11.txt") >>= logShow
