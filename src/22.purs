module Day22 where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Either (Either)
import Data.List (List, concat, mapWithIndex)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (many1, sepBy1)
import Text.Parsing.StringParser.String (char)

type Coord = Tuple Int Int
data InfectedStatus = Infected | Clean | Weakened | Flagged
type Input = Map.Map Coord InfectedStatus

parseCell :: Parser InfectedStatus
parseCell = (char '.' $> Clean) <|> (char '#' $> Infected)

parseRow :: Parser (List InfectedStatus)
parseRow = many1 parseCell

listsToCoordMap :: List (List InfectedStatus) -> Input
listsToCoordMap rows = Map.fromFoldable $ concat $ mapWithIndex (\row rowCells -> mapWithIndex (\col cell -> Tuple (row /\ col) cell) rowCells) rows

parseString :: String -> Either ParseError Input
parseString = runParser $ listsToCoordMap <$> (sepBy1 parseRow (char '\n'))

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)


---

data Direction = North | South | East | West
turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft South = East
turnLeft West = South
turnLeft East = North
turnRight :: Direction -> Direction
turnRight North = East
turnRight South = West
turnRight West = North
turnRight East = South
reverseDirection :: Direction -> Direction
reverseDirection North = South
reverseDirection South = North
reverseDirection West  = East
reverseDirection East  = West

type TravesalState = {
  grid :: Map.Map Coord InfectedStatus,
  direction :: Direction,
  position :: Coord,
  burstCount :: Int,
  infectedCount :: Int
}

moveForward :: Coord -> Direction -> Coord
moveForward (row /\ col) South = ((row+1) /\ col)
moveForward (row /\ col) North = ((row-1) /\ col)
moveForward (row /\ col) East  = (row /\ (col+1))
moveForward (row /\ col) West  = (row /\ (col-1))

getInfectedStatus :: Map.Map Coord InfectedStatus -> Coord -> InfectedStatus
getInfectedStatus m c = fromMaybe Clean $ Map.lookup c m

traverse :: Map.Map Coord InfectedStatus -> Int -> TravesalState
traverse startingGrid bursts = tailRec go initialState
  where initialState = {
          grid: startingGrid,
          direction: North,
          position: (12 /\ 12),
          burstCount: 0,
          infectedCount: 0
        }
        go state = if state.burstCount == bursts then Done state else case getInfectedStatus state.grid state.position of
          Clean -> let newDirection = turnLeft state.direction
                       newGrid = Map.insert state.position Infected state.grid
                       newPosition = moveForward state.position newDirection
                   in Loop {
                     grid: newGrid,
                     direction: newDirection,
                     position: newPosition,
                     burstCount: state.burstCount + 1,
                     infectedCount: state.infectedCount + 1
                   }
          Infected -> let newDirection = turnRight state.direction
                          newGrid = Map.insert state.position Clean state.grid
                          newPosition = moveForward state.position newDirection
                      in Loop {
                         grid: newGrid,
                         direction: newDirection,
                         position: newPosition,
                         burstCount: state.burstCount + 1,
                         infectedCount: state.infectedCount
                      }
          _ -> Done state

tenThousand = 10000
solve :: Input -> Int
solve input = _.infectedCount $ traverse input tenThousand
solve' = (map <<< map) solve (getInput "./src/22.txt") >>= logShow

--------

traverse2 :: Map.Map Coord InfectedStatus -> Int -> TravesalState
traverse2 startingGrid bursts = tailRec go initialState
  where initialState = {
          grid: startingGrid,
          direction: North,
          -- position: (1 /\ 1),
          position: (12 /\ 12),
          burstCount: 0,
          infectedCount: 0
        }
        go state = if state.burstCount == bursts then Done state else case getInfectedStatus state.grid state.position of
          Clean -> let newDirection = turnLeft state.direction
                       newGrid = Map.insert state.position Weakened state.grid
                       newPosition = moveForward state.position newDirection
                   in Loop {
                     grid: newGrid,
                     direction: newDirection,
                     position: newPosition,
                     burstCount: state.burstCount + 1,
                     infectedCount: state.infectedCount
                   }
          Weakened -> let newGrid = Map.insert state.position Infected state.grid
                          newPosition = moveForward state.position state.direction
                      in Loop {
                         grid: newGrid,
                         direction: state.direction,
                         position: newPosition,
                         burstCount: state.burstCount + 1,
                         infectedCount: state.infectedCount + 1
                      }
          Infected -> let newDirection = turnRight state.direction
                          newGrid = Map.insert state.position Flagged state.grid
                          newPosition = moveForward state.position newDirection
                      in Loop {
                         grid: newGrid,
                         direction: newDirection,
                         position: newPosition,
                         burstCount: state.burstCount + 1,
                         infectedCount: state.infectedCount
                      }
          Flagged -> let newDirection = reverseDirection state.direction
                         newGrid = Map.insert state.position Clean state.grid
                         newPosition = moveForward state.position newDirection
                     in Loop {
                       grid: newGrid,
                       direction: newDirection,
                       position: newPosition,
                       burstCount: state.burstCount + 1,
                       infectedCount: state.infectedCount
                     }

alot = 10000000
solve2 :: Input -> Int
solve2 input = _.infectedCount $ traverse2 input alot
solve2' = (map <<< map) solve2 (getInput "./src/22.txt") >>= logShow
