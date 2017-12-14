module Day10 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Char (toCharCode)
import Data.Either (Either)
import Data.Foldable (length, product)
import Data.Int (hexadecimal, toStringAs)
import Data.Int.Bits (xor)
import Data.List (List(..), drop, foldl, fromFoldable, range, reverse, take, (:))
import Data.String (charCodeAt, fromCharArray, joinWith, toCharArray, trim)
import Data.String as String
import Day8 (parseSignedInt)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError(..), runParser)
import Text.Parsing.StringParser.Combinators (sepBy1)
import Text.Parsing.StringParser.String (char)

type Input = List Int

parseString :: String -> Either ParseError Input
parseString = runParser $ sepBy1 parseSignedInt (char ',')


getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

------

type Length = Int
type Position = Int
type SkipSize = Int
data State = State {
  position :: Position,
  skipSize :: SkipSize,
  xs :: List Int
}

instance showState :: Show State where
  show (State s) = (show $ Array.fromFoldable s.xs) <> " (Position: " <> show s.position <> ", Skip Size: " <> show s.skipSize <> ")"

reverseInner :: forall a. List a -> Position -> Length -> List a
reverseInner xs p l = start <> reversed <> end
  where start = take p xs
        toReverse = take l $ drop p xs
        reversed = reverse toReverse
        end = drop (p + l) xs

reverseWrapAround :: forall a. List a -> Position -> Length -> List a
reverseWrapAround xs p l = newStart <> middle <> newEnd
  where toTakeFromStart = (p + l) - length xs
        toTakeFromEnd = length xs - p
        middle = drop toTakeFromStart $ take p xs
        toReverse = end <> start
        start = take toTakeFromStart xs
        end = drop p xs
        reversed = reverse toReverse
        newStart = drop toTakeFromEnd reversed
        newEnd = take toTakeFromEnd reversed

circleReverse :: forall a. List a -> Position -> Length -> List a
circleReverse xs p l = if p + l > length xs then reverseWrapAround xs p l else reverseInner xs p l

performKnot :: State -> Length -> State
performKnot state@(State s) l = State {position: newPosition, skipSize: s.skipSize + 1, xs: ys}
  where ys = circleReverse s.xs s.position l
        newPosition = mod (l + s.position + s.skipSize) (length s.xs)

performKnots :: State -> List Length -> State
performKnots initialState ls = foldl performKnot initialState ls

solve :: Input -> Int
solve input = product $ take 2 $ (\(State s) -> s.xs) (performKnots initialState input)
  where ys = range 0 255
        initialState = State {position: 0, skipSize: 0, xs: ys}

solve' = (map <<< map) (solve) (getInput "./src/10.txt") >>= logShow

-----------

getInput2 :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Array Char)
getInput2 filename =
  map (toCharArray <<< trim) (readTextFile UTF8 filename)

chunk :: forall a. Int -> List a -> List (List a)
chunk n Nil = Nil
chunk n xs = take n xs : (chunk n (drop n xs))

collapse :: List Int -> Int
collapse = foldl xor 0

sparseHashToDenseHash :: List Int -> List Int
sparseHashToDenseHash xs = map collapse (chunk 16 xs)

padLeft :: Char -> Int -> String -> String
padLeft c n s = padding <> s
  where padding = fromCharArray $ Array.replicate m c
        m = (n - String.length s)

denseHashToString :: List Int -> String
denseHashToString xs = (joinWith "" <<< Array.fromFoldable) $ map (padLeft '0' 2 <<< toStringAs hexadecimal) xs

knotHash :: Array Char -> String
knotHash cs = (denseHashToString <<< sparseHashToDenseHash) sparseHash
  where sparseHash = (\(State s) -> s.xs) $ foldl (\s _ -> performKnots s ls) initialState (range 0 63)
        ls = fromFoldable $ map toCharCode cs <> extra
        extra = [17, 31, 73, 47, 23]
        xs = range 0 255
        initialState = State {position: 0, skipSize: 0, xs: xs}

solve2 :: Array Char -> String
solve2 cs = knotHash cs

solve2' = map solve2 (getInput2 "./src/10.txt") >>= logShow
