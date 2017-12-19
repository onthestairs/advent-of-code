module Day19 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (elem, length, snoc, takeWhile, (!!))
import Data.Char (toCharCode)
import Data.Either as Either
import Data.Maybe (Maybe(..), fromJust)
import Data.String (Pattern(..), fromCharArray, split, toCharArray, trim)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy, trace)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)

type Grid = Array (Array Cell)
data Cell =
    Empty
  | VerticlePipe
  | HorizontalPipe
  | Letter Char
  | Corner
derive instance eqCell :: Eq Cell
type Coord = Tuple Int Int
data Direction = Up | Down | Left | Right
derive instance eqDirection :: Eq Direction

parseCell :: Char -> Cell
parseCell '|' = VerticlePipe
parseCell '-' = HorizontalPipe
parseCell '+' = Corner
parseCell c = if toCharCode c >= toCharCode 'A' && toCharCode c <= toCharCode 'Z' then Letter c else Empty

parseInput :: String -> Grid
parseInput s = map (\line -> map parseCell (toCharArray line)) lines
  where lines = split (Pattern "\n") s

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) Grid
getInput filename =
  map (parseInput) (readTextFile UTF8 filename)

------

getCell :: Grid -> Coord -> Maybe Cell
getCell g (row /\ col) = do
  line <- (g !! row)
  line !! col

findStartingCoord :: Grid -> Coord
findStartingCoord g = (0 /\ col)
  where firstLine = unsafePartial $ fromJust (g !! 0)
        col = (length $ takeWhile ((==) Empty) firstLine)

nextCoord :: Coord -> Direction -> Coord
nextCoord (row /\ col) Up = (row-1) /\ col
nextCoord (row /\ col) Down = (row+1) /\ col
nextCoord (row /\ col) Left = row /\ (col-1)
nextCoord (row /\ col) Right = row /\ (col+1)


findNextCoordAndDirection :: Grid -> Coord -> Direction -> Maybe { coord :: Coord, direction :: Direction }
findNextCoordAndDirection grid coord direction =
  if direction == Up || direction == Down
  then case (getCell grid $ nextCoord coord Left) of
       Just HorizontalPipe -> Just {coord: nextCoord coord Left, direction: Left}
       Just (Letter _) -> Just {coord: nextCoord coord Left, direction: Left}
       otherwise -> case (getCell grid $ nextCoord coord Right) of
          Just HorizontalPipe -> Just {coord: nextCoord coord Right, direction: Right}
          Just (Letter _) -> Just {coord: nextCoord coord Right, direction: Right}
          otherwise -> Nothing
  else case (getCell grid $ nextCoord coord Up) of
       Just VerticlePipe -> Just {coord: nextCoord coord Up, direction: Up}
       Just (Letter _) -> Just {coord: nextCoord coord Up, direction: Up}
       otherwise -> case (getCell grid $ nextCoord coord Down) of
          Just VerticlePipe -> Just {coord: nextCoord coord Down, direction: Down}
          Just (Letter _) -> Just {coord: nextCoord coord Down, direction: Down}
          otherwise -> Nothing

showCoord :: Coord -> String
showCoord (row /\ col) = "(" <> show row  <> "," <> show col <> ")"

type State = {
  currentCoord :: Coord,
  direction :: Direction,
  seenLetters :: Array Char,
  pathLength :: Int
}

type AnswerState = {
  seenLetters :: Array Char,
  pathLength :: Int
}

followGrid :: Grid -> State -> Either.Either String AnswerState
followGrid grid initialState = tailRec go initialState
  where go state = case getCell grid state.currentCoord of
                    Nothing -> Done $ Either.Left "Entered cell off the grid" -- this is an illegal state
                    Just VerticlePipe -> Loop $ state { currentCoord = nextCoord state.currentCoord state.direction, pathLength = state.pathLength + 1 }
                    Just HorizontalPipe -> Loop $ state { currentCoord = nextCoord state.currentCoord state.direction, pathLength = state.pathLength + 1 }
                    Just (Letter c) -> Loop $ state { currentCoord = nextCoord state.currentCoord state.direction, seenLetters = snoc state.seenLetters c, pathLength = state.pathLength + 1 }
                    Just Empty -> Done $ Either.Right {seenLetters: state.seenLetters, pathLength: state.pathLength} -- entered empty cell (i.e. finished)
                    Just Corner -> case findNextCoordAndDirection grid state.currentCoord state.direction of
                                      Just { coord, direction } -> Loop $ state { currentCoord = coord, direction = direction, pathLength = state.pathLength + 1 }
                                      Nothing -> Done $ Either.Right {seenLetters: state.seenLetters, pathLength: state.pathLength}


solve :: Grid -> Either.Either String String
solve grid = map (fromCharArray <<< _.seenLetters) (followGrid grid initialState)
  where initialState = {currentCoord: findStartingCoord grid, direction: Down, seenLetters: [], pathLength: 0}

solve' = (map) solve (getInput "./src/19.txt") >>= logShow

----

solve2 :: Grid -> Either.Either String Int
solve2 grid = map _.pathLength (followGrid grid initialState)
  where initialState = {currentCoord: findStartingCoord grid, direction: Down, seenLetters: [], pathLength: 0}

solve2' = (map) solve2 (getInput "./src/19.txt") >>= logShow
