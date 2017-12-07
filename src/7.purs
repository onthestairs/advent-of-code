module Day7 where

import Prelude

import Control.Apply (lift2)
import Control.Comonad.Cofree (head, tail, unfoldCofree, (:<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Foldable (length, sum)
import Data.Graph (Graph, fromMap, topologicalSort)
import Data.List (filter, sortBy)
import Data.List as List
import Data.List.Lazy as LazyList
import Data.List.NonEmpty as NonEmptyList
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.String (fromCharArray, trim)
import Data.Tree (Tree)
import Data.Tuple.Nested ((/\))
import Day2 (parseIntChars, readInt)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (between, many1, option, sepBy1)
import Text.Parsing.StringParser.String (char, lowerCaseChar, string)

type ProgramName = String
type Weight = Int
type ProgramInfo = {
  name :: ProgramName,
  weight :: Weight,
  dependants :: Array String
}
type Input = Array ProgramInfo

parseInt :: Parser Int
parseInt = map readInt parseIntChars

parseProgramName :: Parser ProgramName
parseProgramName = map (fromCharArray <<< fromFoldable) $ many1 lowerCaseChar

parseDependants :: Parser (Array ProgramName)
parseDependants = do
  _ <- char ' '
  _ <- string "->"
  _ <- char ' '
  programs <- fromFoldable <$> sepBy1 parseProgramName (string ", ")
  pure programs


line :: Parser ProgramInfo
line = do
  name <- parseProgramName
  _ <- char ' '
  weight <- between (char '(') (char ')') parseInt
  dependants <- option [] parseDependants
  pure $ {
    name: name,
    weight: weight,
    dependants: dependants
  }

lines :: Parser Input
lines = fromFoldable <$> sepBy1 line (char '\n')

parseString :: String -> Either ParseError Input
parseString input = runParser lines input

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)


----
toGraph :: Input -> Graph String String
toGraph ps = fromMap $ Map.fromFoldable $ map makeMapTuple ps
  where makeMapTuple p = p.name /\ (p.name /\ (List.fromFoldable p.dependants))

solve :: Input -> Maybe ProgramName
solve ps = List.head $ topologicalSort (toGraph ps)

solve' = (map <<< map) solve (getInput "./src/7.txt") >>= logShow

-----

newtype NameAndWeight = NameAndWeight {name :: String, nodeWeight :: Int}
instance showNameAndWeight :: Show NameAndWeight where
  show (NameAndWeight {name, nodeWeight}) = name <> " - " <> show nodeWeight

newtype NameAndWeightAndTowerWeights = NameAndWeightAndTowerWeights {name :: String, nodeWeight :: Int, towerWeight :: Int}
extractTowerWeight (NameAndWeightAndTowerWeights w) = w.towerWeight
extractWeight (NameAndWeightAndTowerWeights w) = w.nodeWeight
instance showNameAndWeightAndTowerWeights :: Show NameAndWeightAndTowerWeights where
  show (NameAndWeightAndTowerWeights {name, nodeWeight, towerWeight}) = name <> " - " <> show towerWeight <> " (" <> show nodeWeight <> ")"

makeTowerTree :: Array ProgramInfo -> Tree NameAndWeight
makeTowerTree ps = unfoldCofree id f (NameAndWeight {name: startName, nodeWeight: startWeight})
  where m = Map.fromFoldable $ map (\p -> p.name /\ {weight: p.weight, dependants: List.fromFoldable p.dependants}) ps
        startName = fromMaybe "" $ solve ps
        startWeight = fromMaybe 0 $ _.weight <$> Map.lookup startName m
        f (NameAndWeight {name, nodeWeight}) = map (\n ->  NameAndWeight {name: n, nodeWeight: getWeight n}) $ fromMaybe List.Nil $ _.dependants <$> Map.lookup name m
        getWeight name = fromMaybe 0 $ _.weight <$> Map.lookup name m


addTowerWeights :: Tree NameAndWeight -> Tree NameAndWeightAndTowerWeights
addTowerWeights t = NameAndWeightAndTowerWeights {name: x.name, nodeWeight: x.nodeWeight, towerWeight: x.nodeWeight + parentsWeight} :< ys
  where (NameAndWeight x) = head t
        ys = map addTowerWeights (tail t)
        parentsWeight = sum $ map (extractTowerWeight <<< head) ys

allEqualBy :: forall a b. Eq b => (a -> b) -> List.List a -> Boolean
allEqualBy e List.Nil = true
allEqualBy e (List.Cons x xs) = List.all ((==) (e x)) (map e xs)

layers :: forall a. Tree a -> List.List (List.List (List.List a))
layers t = List.fromFoldable $ map (map (map head)) $ LazyList.takeWhile (not <<< List.null) $ LazyList.iterate (\xss -> map tail (List.concat xss)) (List.singleton (List.singleton t))

findErrantBy :: forall a b. Eq b => (a -> b) -> List.List (List.List (List.List a)) -> List.List (List.List (List.List a))
findErrantBy e xsss = map (filter (not <<< allEqualBy e)) xsss

calculateNewWeight :: List.List NameAndWeightAndTowerWeights -> Maybe Int
calculateNewWeight xs = lift2 (+) (extractWeight <$> rogue) difference
  where gs = List.groupBy (\a b -> extractTowerWeight a == extractTowerWeight b) (sortBy (comparing extractTowerWeight) xs)
        rogue = NonEmptyList.head <$> List.head (filter (\g -> length g == 1) gs)
        nonRogue = NonEmptyList.head <$> List.head (filter (\g -> length g > 1) gs)
        difference = lift2 (-) (extractTowerWeight <$> nonRogue) (extractTowerWeight <$> rogue)

findAnswer :: List.List (List.List (List.List NameAndWeightAndTowerWeights)) -> Maybe Int
findAnswer xsss = xs >>= calculateNewWeight
  where xs = List.last $ (List.concat) xsss

solve2 = findAnswer <<< findErrantBy extractTowerWeight <<< layers <<< addTowerWeights <<< makeTowerTree
solve2' = (map <<< map) solve2 (getInput "./src/7.txt") >>= logShow
