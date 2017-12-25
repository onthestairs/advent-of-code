module Day25 where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Either (Either)
import Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Day2 (parseInt)
import Debug.Trace (spy)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (sepBy1)
import Text.Parsing.StringParser.String (char, skipSpaces, string)

data State = A | B | C | D | E | F
derive instance eqState :: Eq State
derive instance ordState :: Ord State
data Direction = Left | Right
data Bit = Zero | One
derive instance eqBit :: Eq Bit
data Instruction = Instruction Bit Direction State
data Rule = Rule Instruction Instruction
getInstruction0 :: Rule -> Instruction
getInstruction0 (Rule i0 i1) = i0
getInstruction1 :: Rule -> Instruction
getInstruction1 (Rule i0 i1) = i1
type Rules = Map.Map State Rule
type Input = {
  startingState :: State,
  stepsToPerform :: Int,
  rules :: Rules
}

parseState :: Parser State
parseState =
  (char 'A' $> A) <|>
  (char 'B' $> B) <|>
  (char 'C' $> C) <|>
  (char 'D' $> D) <|>
  (char 'E' $> E) <|>
  (char 'F' $> F)

parseBit :: Parser Bit
parseBit =
  (char '0' $> Zero) <|>
  (char '1' $> One)

parseDirection :: Parser Direction
parseDirection =
  (string "right" $> Right) <|>
  (string "left" $> Left)


parseRule :: Parser (Tuple State Rule)
parseRule = do
  _ <- string "In state "
  state <- parseState
  _ <- string ":"
  skipSpaces
  _ <- string "If the current value is 0:"
  skipSpaces
  _ <- string "- Write the value "
  bit0 <- parseBit
  _ <- string "."
  skipSpaces
  _ <- string "- Move one slot to the "
  direction0 <- parseDirection
  _ <- string "."
  skipSpaces
  _ <- string "- Continue with state "
  state0 <- parseState
  _ <- string "."
  skipSpaces
  _ <- string "If the current value is 1:"
  skipSpaces
  _ <- string "- Write the value "
  bit1 <- parseBit
  _ <- string "."
  skipSpaces
  _ <- string "- Move one slot to the "
  direction1 <- parseDirection
  _ <- string "."
  skipSpaces
  _ <- string "- Continue with state "
  state1 <- parseState
  _ <- string "."
  let instruction0 = Instruction bit0 direction0 state0
  let instruction1 = Instruction bit1 direction1 state1
  pure $ Tuple state (Rule instruction0 instruction1)

parseRules :: Parser Rules
parseRules = Map.fromFoldable <$> sepBy1 parseRule (string "\n\n")

parseInput :: Parser Input
parseInput = do
  _ <- string "Begin in state "
  state <- parseState
  _ <- string "."
  skipSpaces
  _ <- string "Perform a diagnostic checksum after "
  stepsToPerform <- parseInt
  _ <- string " steps."
  skipSpaces
  rules <- parseRules
  pure {startingState: state, stepsToPerform: stepsToPerform, rules: rules}

parseString :: String -> Either ParseError Input
parseString input = runParser parseInput input

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

----

type Tape = Map.Map Int Bit

getTapeValue :: Tape -> Int -> Bit
getTapeValue t s = fromMaybe Zero $ Map.lookup s t

setTapeValue :: Tape -> Int -> Bit -> Tape
setTapeValue t s Zero = Map.delete s t
setTapeValue t s One = Map.insert s One t

tapeSize :: Tape -> Int
tapeSize = Map.size

move :: Int -> Direction -> Int
move n Left = n - 1
move n Right = n + 1

step rules state =
  {
    steps: state.steps + 1,
    cursor: move state.cursor directionToMove,
    tape: setTapeValue state.tape state.cursor bitToWrite,
    currentState: stateToMoveTo
  }
  where rule = unsafePartial $ fromJust $ Map.lookup state.currentState rules
        currentBit = getTapeValue state.tape state.cursor
        (Instruction bitToWrite directionToMove stateToMoveTo) = if currentBit == Zero then getInstruction0 rule else getInstruction1 rule

runTape :: Input -> Tape
runTape {startingState, stepsToPerform, rules} = tailRec go initialState
  where initialState = {
          steps: 0,
          cursor: 0,
          tape: Map.empty,
          currentState: startingState
        }
        go state =
          if state.steps == stepsToPerform
          then Done state.tape
          else Loop $ step rules state

solve input = tapeSize $ runTape input

solve' = (map <<< map) solve (getInput "./src/25.txt") >>= logShow
