module Day24 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (tailRec)
import Data.Either (Either)
import Data.Foldable (maximum, sum)
import Data.List (List(..), concatMap, drop, filter, fromFoldable, length, singleton, span, (:))
import Data.Maybe (Maybe)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Day2 (parseInt)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (sepBy1)
import Text.Parsing.StringParser.String (char)

data Component = Component Int Int
derive instance eqComponent :: Eq Component
instance showComponent :: Show Component where
  show (Component i j) = show i <> "/" <> show j
rotate (Component i j) = Component j i
fst (Component i j) = i
snd (Component i j) = j
strength (Component i j) = i + j
type Input = List Component


parseComponent :: Parser Component
parseComponent = do
  i <- parseInt
  _ <- char '/'
  j <- parseInt
  pure (Component i j)

parseString :: String -> Either ParseError Input
parseString input = runParser (fromFoldable <$> sepBy1 parseComponent (char '\n')) input

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

-----

takeElem :: forall a. Eq a => List a -> a -> {c :: a, rest :: List a}
takeElem cs c = case span ((/=) c) cs of
  {init, rest} -> {c: c, rest: init <> (drop 1 rest)}

validComponents :: Int -> List Component -> List {c :: Component, rest :: List Component}
validComponents n cs = map (takeElem rotated) $ filter (\c -> fst c == n) rotated
  where rotated = map (\c -> if fst c == n then c else rotate c) cs

makeBridges :: Int -> List Component -> List (List Component)
makeBridges n cs = concatMap f possibleComponents
  where possibleComponents = validComponents n cs
        f {c, rest} = let ds = makeBridges (snd c) rest in if length ds == 0 then singleton (singleton c) else map (\d -> c:d) ds

solve :: Input -> Maybe Int
solve input = maximum $ map (sum <<< map strength) (makeBridges 0 input)

solve' = (map <<< map) solve (getInput "./src/24.txt") >>= logShow

---------

solve2 :: Input -> Maybe (Tuple Int Int)
solve2 input = maximum $ map (\bridge -> Tuple (length bridge) (sum $ map strength bridge)) (makeBridges 0 input)

solve2' = (map <<< map) solve2 (getInput "./src/24.txt") >>= logShow
