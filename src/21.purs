module Day21 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (catMaybes, concatMap, fromFoldable, head, nub)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (length)
import Data.List
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid (class Monoid)
import Data.String (Pattern(..), fromCharArray, trim)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (many1, sepBy1)
import Text.Parsing.StringParser.String (char, oneOf, string)

data Grid a = Quartet a a a a | Nonet a a a a a a a a a
instance showGrid :: (Show a) => Show (Grid a) where
  show (Quartet a1 a2 a3 a4) = "Quartet " <> show a1 <> " " <> show a2 <> " " <> show a3 <> " " <> show a4
  show (Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9) = "Nonet " <> show a1 <> " " <> show a2 <> " " <> show a3 <> " " <> show a4 <> show a5 <> " " <> show a6 <> " " <> show a7 <> " " <> show a8 <> " " <> show a9
instance eqGrid :: (Eq a) => Eq (Grid a) where
  eq (Quartet a1 a2 a3 a4) (Quartet b1 b2 b3 b4) = (a1 == b1) && (a2 == b2) && (a3 == b3) && (a4 == b4)
  eq (Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9) (Nonet b1 b2 b3 b4 b5 b6 b7 b8 b9) = (a1 == b1) && (a2 == b2) && (a3 == b3) && (a4 == b4) && (a5 == b5) && (a6 == b6) && (a7 == b7) && (a8 == b8) && (a9 == b9)
  eq (Quartet _ _ _ _) (Nonet _ _ _ _ _ _ _ _ _) = false
  eq (Nonet _ _ _ _ _ _ _ _ _) (Quartet _ _ _ _) = false

toString :: Grid Char -> String
toString (Quartet a1 a2 a3 a4) = fromCharArray [
  a1, a2, '/',
  a3, a4]
toString (Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9) = fromCharArray [
  a1, a2, a3, '/',
  a4, a5, a6, '/',
  a7, a8, a9]

flips :: forall a. Grid a -> Array (Grid a)
-- 1 2
-- 3 4
flips (Quartet a1 a2 a3 a4) = [
  Quartet a2 a1 a4 a3,
  Quartet a3 a1 a4 a2
]
-- 1 2 3
-- 4 5 6
-- 7 8 9
flips (Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9) = [
  Nonet a3 a2 a1 a6 a5 a4 a9 a8 a7,
  Nonet a7 a8 a9 a4 a5 a6 a1 a2 a3
]

rotate1 :: forall a. Grid a -> Grid a
-- 1 2
-- 3 4
rotate1 (Quartet a1 a2 a3 a4) = Quartet a3 a1 a4 a2
-- 1 2 3
-- 4 5 6
-- 7 8 9
rotate1 (Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9) = Nonet a7 a4 a1 a8 a5 a2 a9 a6 a3

rotations :: forall a. Grid a -> Array (Grid a)
rotations g = [rotate1 g, rotate1 (rotate1 g), rotate1 (rotate1 (rotate1 g))]

variations :: forall a. Eq a => Grid a -> Array (Grid a)
variations g = nub $ [g] <> flips g <> rotations g <> (concatMap flips (rotations g))

----

type Rule = Tuple String String
type Input = Map.Map String String

parseGridString :: Parser String
parseGridString = String.fromCharArray <$> fromFoldable <$> many1 (oneOf ['.', '#', '/'])

parseRule :: Parser Rule
parseRule = do
  from <- parseGridString
  _ <- string " => "
  to <- parseGridString
  pure (Tuple from to)

parseString :: String -> Either ParseError Input
parseString = runParser $ Map.fromFoldable <$> (sepBy1 parseRule (char '\n'))

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

----------

chunk2 :: forall a. Partial => List a -> List (Tuple a a)
chunk2 Nil = Nil
chunk2 (x:y:xs) = (Tuple x y) : chunk2 xs

chunk3 :: forall a. Partial => List a -> List (Tuple a (Tuple a a))
chunk3 Nil = Nil
chunk3 (x:y:z:xs) = (Tuple x (Tuple y z)) : (chunk3 xs)

decompose2 :: Partial => String -> List (List (Grid Char))
decompose2 s = quartets
  where lines = map (List.fromFoldable <<< String.toCharArray) $ List.fromFoldable $ String.split (Pattern "/") s
        pairs = chunk2 lines
        quartets = map (\(Tuple line1 line2) -> zipWith (\(Tuple a1 a2) (Tuple a3 a4) -> Quartet a1 a2 a3 a4) (chunk2 line1) (chunk2 line2)) pairs

zipWith3 :: forall a b c d. (a -> b -> c -> d) -> List a -> List b -> List c -> List d
zipWith3 f as bs cs = map (\(Tuple (Tuple a b) c) -> f a b c) $ zip (zip as bs) cs

decompose3 :: Partial => String -> List (List (Grid Char))
decompose3 s = nonets
  where lines :: List (List Char)
        lines = map (List.fromFoldable <<< String.toCharArray) $ List.fromFoldable $ String.split (Pattern "/") s
        triples :: List (Tuple (List Char) (Tuple (List Char) (List Char)))
        triples = chunk3 lines
        nonets :: List (List (Grid Char))
        nonets = map makeNonets triples
        makeNonets :: (Tuple (List Char) (Tuple (List Char) (List Char))) -> List (Grid Char)
        makeNonets (Tuple line1 (Tuple line2 line3)) = zipWith3 (\(a1 /\ a2 /\ a3) (a4 /\ a5 /\ a6) (a7 /\ a8 /\ a9) -> Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9) (chunk3 line1) (chunk3 line2) (chunk3 line3)

decompose :: String -> List (List (Grid Char))
decompose s =
  if length lines `mod` 2 == 0
  then unsafePartial $ decompose2 s
  else unsafePartial $ decompose3 s
  where lines = String.split (Pattern "/") s

zipN :: forall a. Monoid a => List (List a) -> List a
zipN Nil = Nil
zipN (xs:Nil) = xs
zipN (xs:xss) = zipWith (\a b -> a <> b) xs (zipN xss)

recomposeRow :: List String -> String
recomposeRow blocks = String.joinWith "/" (Array.fromFoldable $ zipN splitBlocks)
  where splitBlocks :: List (List String)
        splitBlocks = map (List.fromFoldable <<< String.split (Pattern "/")) blocks

recompose :: List (List String) -> String
recompose xss = String.joinWith "/" (Array.fromFoldable $ map recomposeRow xss)

step :: (Grid Char -> String) -> String -> String
step f s = recompose $ (map <<< map) f $ decompose s

startingGrid :: String
startingGrid = ".#./..#/###"

repeatF :: forall a. (a -> a) -> Int -> (a -> a)
repeatF f 0 = id
repeatF f n = f <<< repeatF f (n-1)

solveAfterN :: Input -> Int -> Int
solveAfterN rules n = length (Array.filter ((==) '#') (String.toCharArray finalGrid))
  where f :: (Grid Char -> String)
        f g = unsafePartial $ fromJust $ head $ catMaybes $ map (flip Map.lookup rules) $ candidates
          where candidates = map toString $ variations g
        step' :: String -> String
        step' = step f
        finalGrid = (repeatF step' n) startingGrid

solve :: Input -> Int
solve rules = solveAfterN rules 5
solve' = (map <<< map) solve (getInput "./src/21.txt") >>= logShow

---

solve2 :: Input -> Int
solve2 rules = solveAfterN rules 18
solve2' = (map <<< map) solve2 (getInput "./src/21.txt") >>= logShow
