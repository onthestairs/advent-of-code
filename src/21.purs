module Day21 where

import Prelude

import Control.Comonad.Store (seek)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (catMaybes, concatMap, filter, fromFoldable, head, nub, range, scanl, zip)
import Data.Either (Either(..))
import Data.Foldable (length, traverse_)
import Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.String (fromCharArray, joinWith, trim)
import Data.String as String
import Data.String.Utils (lines, repeat)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, (/\))
import Debug.Trace (spy, trace, traceShow)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (many1, sepBy1)
import Text.Parsing.StringParser.String (char, oneOf, string)

data Quartet a = Quartet a a a a
data Nonet a = Nonet a a a a a a a a a

toStringQuartet :: Quartet Char -> String
toStringQuartet (Quartet a1 a2 a3 a4) = fromCharArray [
  a1, a2, '/',
  a3, a4]

toStringNonet :: Nonet Char -> String
toStringNonet (Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9) = fromCharArray [
  a1, a2, a3, '/',
  a4, a5, a6, '/',
  a7, a8, a9]

data Grid  =
    OuterQuartet (Quartet Grid)
  | OuterNonet (Nonet Grid)
  | InnerQuartet (Quartet Char)
  | InnerNonet (Nonet Char)
instance showGridI :: Show Grid where
  show = showGrid

showGrid :: Grid -> String
-- showGrid (OuterQuartet (Quartet a1 a2 a3 a4)) =
--   top <> divider <> bottom
--   where top = (joinWith "" $ map (\(Tuple a1 a2) -> a1 <> "|" <> a2 <> "\n") (zip (lines (showGrid a1)) (lines (showGrid a2))))
--         bottom = (joinWith "" $ map (\(Tuple a1 a2) -> a1 <> "|" <> a2 <> "\n") (zip (lines (showGrid a3)) (lines (showGrid a4))))
--         dividingLength = 1 + String.length top / (length $ lines top)
--         divider = (unsafePartial $ fromJust $ repeat dividingLength "-") <> "\n"
showGrid (OuterQuartet (Quartet a1 a2 a3 a4)) =
  top <> bottom
  where top = (joinWith "" $ map (\(Tuple a1 a2) -> a1 <> a2 <> "\n") (zip (lines (showGrid a1)) (lines (showGrid a2))))
        bottom = (joinWith "" $ map (\(Tuple a1 a2) -> a1 <> a2 <> "\n") (zip (lines (showGrid a3)) (lines (showGrid a4))))
        -- dividingLength = 1 + String.length top / (length $ lines top)
        -- divider = (unsafePartial $ fromJust $ repeat dividingLength "-") <> "\n"
showGrid (OuterNonet (Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9)) = "Never seen"
  -- (joinWith "" $ map (\((a1 /\ a2) /\ a3) -> a1 <> "|" <> a2 <> "|" <> a3 <> "\n") (zip (zip (lines (showGrid a1)) (lines (showGrid a2))) (lines (showGrid a3)))) <> "--\n" <>
  -- (joinWith "" $ map (\((a1 /\ a2) /\ a3) -> a1 <> "|" <> a2 <> "|" <> a3 <> "\n") (zip (zip (lines (showGrid a4)) (lines (showGrid a5))) (lines (showGrid a6)))) <> "--\n" <>
  -- (joinWith "" $ map (\((a1 /\ a2) /\ a3) -> a1 <> "|" <> a2 <> "|" <> a3 <> "\n") (zip (zip (lines (showGrid a7)) (lines (showGrid a8))) (lines (showGrid a9))))
showGrid (InnerQuartet (Quartet a1 a2 a3 a4)) = fromCharArray [a1, a2, '\n', a3, a4]
showGrid (InnerNonet (Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9)) = fromCharArray [a1, a2, a3, '\n', a4, a5, a6, '\n', a7, a8, a9]

toString :: Grid -> String
toString g = unsafePartial $
  case g of
    (InnerQuartet q) -> toStringQuartet q
    (InnerNonet n) -> toStringNonet n

step :: (Grid -> Grid) -> Grid -> Grid
step f (OuterQuartet (Quartet a1 a2 a3 a4)) = OuterQuartet (Quartet (step f a1) (step f a2) (step f a3) (step f a4))
step f (OuterNonet (Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9)) = OuterNonet (Nonet (step f a1) (step f a2) (step f a3) (step f a4) (step f a5) (step f a6) (step f a7) (step f a8) (step f a9))
step f g@(InnerQuartet q) = f g
step f g@(InnerNonet q) = f g

step' :: (Grid -> Grid) -> Grid -> Grid
step' f g = traceShow g \_ -> step f g

-- 1 2
-- 3 4
flipsQuartet :: forall a. Quartet a -> Array (Quartet a)
flipsQuartet (Quartet a1 a2 a3 a4) = [
  Quartet a2 a1 a4 a3,
  Quartet a3 a1 a4 a2
]

-- 1 2 3
-- 4 5 6
-- 7 8 9
flipsNonet :: forall a. Nonet a -> Array (Nonet a)
flipsNonet (Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9) = [
  Nonet a3 a2 a1 a6 a5 a4 a9 a8 a7,
  Nonet a7 a8 a9 a4 a5 a6 a1 a2 a3
]

gridFlips :: Grid -> Array Grid
gridFlips (OuterQuartet q) = map OuterQuartet (flipsQuartet q)
gridFlips (OuterNonet n) = map OuterNonet (flipsNonet n)
gridFlips (InnerQuartet q) = map InnerQuartet (flipsQuartet q)
gridFlips (InnerNonet n) = map InnerNonet (flipsNonet n)

-- 1 2
-- 3 4
rotateQuartet1 :: forall a. Quartet a -> Quartet a
rotateQuartet1 (Quartet a1 a2 a3 a4) = Quartet a3 a1 a4 a2

-- 1 2 3
-- 4 5 6
-- 7 8 9
rotateNonet1 :: forall a. Nonet a -> Nonet a
rotateNonet1 (Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9) = Nonet a7 a4 a1 a8 a5 a2 a9 a6 a3

gridRotate1 :: Grid -> Grid
gridRotate1 (OuterQuartet q) = OuterQuartet (rotateQuartet1 q)
gridRotate1 (OuterNonet n) = OuterNonet (rotateNonet1 n)
gridRotate1 (InnerQuartet q) = InnerQuartet (rotateQuartet1 q)
gridRotate1 (InnerNonet n) = InnerNonet (rotateNonet1 n)

gridRotations :: Grid -> Array Grid
gridRotations g = [gridRotate1 g, gridRotate1 (gridRotate1 g), gridRotate1 (gridRotate1 (gridRotate1 g))]

----

type Rule = Tuple String Grid
type Input = Map.Map String Grid

stringToGrid :: String -> Grid
stringToGrid s = unsafePartial $
  case String.toCharArray s of
    [a1, a2, a3, '/', a4, a5, a6, '/', a7, a8, a9] -> InnerNonet (Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9)
    [a1, a2, a3, a4, '/', a5, a6, a7, a8, '/', a9, a10, a11, a12, '/', a13, a14, a15, a16] ->
      let topLeft :: Grid
          topLeft  = InnerQuartet (Quartet a1 a2 a5 a6)
          topRight = InnerQuartet (Quartet a3 a4 a7 a8)
          bottomLeft = InnerQuartet (Quartet a9 a10 a13 a14)
          bottomRight = InnerQuartet (Quartet a11 a12 a15 a16)
      in OuterQuartet (Quartet topLeft topRight bottomLeft bottomRight)

parseGridString :: Parser String
parseGridString = String.fromCharArray <$> fromFoldable <$> many1 (oneOf ['.', '#', '/'])

parseRule :: Parser Rule
parseRule = do
  from <- parseGridString
  _ <- string " => "
  to <- parseGridString
  pure (Tuple from (stringToGrid to))

parseString :: String -> Either ParseError Input
parseString = runParser $ Map.fromFoldable <$> (sepBy1 parseRule (char '\n'))

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

-- .#.
-- ..#
-- ###
startingGrid :: Grid
startingGrid = InnerNonet (Nonet '.' '#' '.' '.' '.' '#' '#' '#' '#')

countOn :: Grid -> Int
countOn (OuterQuartet (Quartet a1 a2 a3 a4)) = countOn a1 + countOn a2 + countOn a3 + countOn a4
countOn (OuterNonet (Nonet a1 a2 a3 a4 a5 a6 a7 a8 a9)) = countOn a1 + countOn a2 + countOn a3 + countOn a4 + countOn a5 + countOn a6 + countOn a7 + countOn a8 + countOn a9
countOn (InnerQuartet q) = length $ filter (\c -> c == '#') $ String.toCharArray $ (toStringQuartet q)
countOn (InnerNonet n) = length $ filter (\c -> c == '#') $ String.toCharArray $ (toStringNonet n)

repeatF :: forall a. (a -> a) -> Int -> (a -> a)
repeatF f 0 = id
repeatF f n = f <<< (repeatF f (n-1))

-- solve m = scanl (\grid i -> h grid) startingGrid (range 0 4)
--   where f :: Grid -> Grid
--         f grid = unsafePartial $ fromJust $ head $ enhancementCandidates
--           where strings = map toString ([grid] <> gridFlips grid <> gridRotations grid <> (concatMap gridFlips $ gridRotations grid))
--                 enhancementCandidates = catMaybes $ map (flip Map.lookup m) strings
--                 -- blah = spy strings
--         h = step f

solve m = repeatF h 2 startingGrid
  where f :: Grid -> Grid
        f grid = unsafePartial $ fromJust $ head $ trace ("string: " <> (show $ head strings) <> ", candidate: " <> show (map show enhancementCandidates)) \_ -> enhancementCandidates
          where strings = nub $ map toString $ ([grid] <> gridFlips grid <> gridRotations grid <> (concatMap gridFlips $ gridRotations grid))
                enhancementCandidates = catMaybes $ map (flip Map.lookup m) strings
        h = step f


-- solve' = (map <<< map) solve (getInput "./src/21.txt") >>= g
--   where g (Right xs) = traverse_ logShow xs
--         g (Left _) = log "hello"
solve' = (map <<< map) solve (getInput "./src/21.txt") >>= showRight

showRight (Right x) = logShow x
showRight (Left _) = log "left :("
