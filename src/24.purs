module Day24 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (concatMap, cons, drop, filter, fromFoldable, length, span)
import Data.Either (Either)
import Data.Foldable (maximum, sum)
import Data.Maybe (Maybe)
import Data.String (trim)
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
type Input = Array Component


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

takeElem :: forall a. Eq a => Array a -> a -> {c :: a, rest :: Array a}
takeElem cs c = case span ((/=) c) cs of
  {init, rest} -> {c: c, rest: init <> (drop 1 rest)}

validComponents :: Int -> Array Component -> Array {c :: Component, rest :: Array Component}
validComponents n cs = map (takeElem rotated) $ filter (\c -> fst c == n) rotated
  where rotated = map (\c -> if fst c == n then c else rotate c) cs

-- makeBridges :: Int -> Array Component -> Array (Array Component)
-- makeBridges n cs = concatMap f possibleComponents
--   where possibleComponents = validComponents n cs
--         f {c, rest} = let ds = makeBridges (snd c) rest in if length ds == 0 then [[c]] else map (cons c) ds

makeBridges :: Int -> Array Component -> Array (Array Component)
makeBridges n cs = concatMap f possibleComponents
  where possibleComponents = validComponents n cs
        f {c, rest} = let ds = makeBridges (snd c) rest in if length ds == 0 then [[c]] else map (cons c) ds

solve :: Input -> Maybe Int
solve input = maximum $ map (sum <<< map strength) (makeBridges 0 input)

solve' = (map <<< map) solve (getInput "./src/24.test.txt") >>= logShow
