module Day18 where

import Prelude

import Control.Alt ((<|>))
import Control.Fold (scanl)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION, name)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (concatMap, fromFoldable, length, nub, null, snoc, uncons, (!!))
import Data.BigInt (BigInt, fromInt, toNumber)
import Data.Either (Either)
import Data.Generic (class Generic, gShow)
import Data.Int (round)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Day8 (parseSignedInt)
import Debug.Trace (spy, trace)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (many1, sepBy1)
import Text.Parsing.StringParser.String (char, lowerCaseChar, skipSpaces, string, whiteSpace)

type Name = Char
data ValueOrReference = Value Int | Reference Name
derive instance genericValueOrReference :: Generic ValueOrReference
instance showValueOrReference :: Show ValueOrReference where
  show (Value n) = show n
  show (Reference name) = "*" <> show name
data Instruction =
    Play ValueOrReference
  | Set Name ValueOrReference
  | Add Name ValueOrReference
  | Multiply Name ValueOrReference
  | Modulo Name ValueOrReference
  | Recover Name
  | Jump ValueOrReference ValueOrReference
derive instance genericInstruction :: Generic Instruction
instance showInstruction :: Show Instruction
  where show = gShow
type Input = Array Instruction

parseName :: Parser Name
parseName = lowerCaseChar

parseValueOrReference :: Parser ValueOrReference
parseValueOrReference =
  (Value <$> parseSignedInt) <|>
  (Reference <$> parseName)

parsePlay :: Parser Instruction
parsePlay = do
  _ <- string "snd"
  skipSpaces
  val <- parseValueOrReference
  pure (Play val)

parseSet :: Parser Instruction
parseSet = do
  _ <- string "set"
  skipSpaces
  name <- parseName
  skipSpaces
  val <- parseValueOrReference
  pure (Set name val)

parseAdd :: Parser Instruction
parseAdd = do
  _ <- string "add"
  skipSpaces
  name <- parseName
  skipSpaces
  val <- parseValueOrReference
  pure (Add name val)

parseMultiply :: Parser Instruction
parseMultiply = do
  _ <- string "mul"
  skipSpaces
  name <- parseName
  skipSpaces
  val <- parseValueOrReference
  pure (Multiply name val)

parseModulo :: Parser Instruction
parseModulo = do
  _ <- string "mod"
  skipSpaces
  name <- parseName
  skipSpaces
  val <- parseValueOrReference
  pure (Modulo name val)

parseRecover :: Parser Instruction
parseRecover = do
  _ <- string "rcv"
  skipSpaces
  name <- parseName
  pure (Recover name)

parseJump :: Parser Instruction
parseJump = do
  _ <- string "jgz"
  skipSpaces
  name <- parseValueOrReference
  skipSpaces
  val <- parseValueOrReference
  pure (Jump name val)

parseInstruction :: Parser Instruction
parseInstruction =
  parsePlay <|>
  parseSet <|>
  parseAdd <|>
  parseMultiply <|>
  parseModulo <|>
  parseRecover <|>
  parseJump

parseString :: String -> Either ParseError Input
parseString = runParser $ fromFoldable <$> (sepBy1 parseInstruction (char '\n'))

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)


-----

type RegistryState = BigInt
type Registry = Map.Map Name RegistryState

type State = {
  instructions :: Array Instruction,
  position :: Int,
  registry :: Registry,
  lastPlayedFrequency :: Maybe BigInt
}

showState :: State -> String
showState s = show m <> " " <> "(" <> show currentInstruction <> ")" <> " " <> show s.position
  where currentInstruction = s.instructions !! s.position
        m = map (\(Tuple k v) -> show k <> ":" <> show v) $ (Map.toUnfoldable s.registry :: Array (Tuple Name RegistryState))

getValue :: Registry -> ValueOrReference -> Maybe BigInt
getValue r (Value n) = Just (fromInt n)
getValue r (Reference name) = Map.lookup name r

modifyName :: Name -> (RegistryState -> RegistryState) -> Registry -> Registry
modifyName name f r = Map.update (Just <<< f) name r

runStepValueInstruction :: forall r. { registry :: Registry, position :: Int | r } -> Name -> ValueOrReference -> (BigInt -> BigInt -> BigInt) -> { registry :: Registry, position :: Int | r }
runStepValueInstruction s name val f = s {registry = newRegistry, position = s.position + 1}
  where value = getValue s.registry val
        newRegistry = case value of
          Nothing -> s.registry
          Just v -> modifyName name (\prev -> f prev v) s.registry

runInstruction :: State -> Instruction -> State
runInstruction s (Play val) = s { lastPlayedFrequency = value, position = s.position + 1 }
  where value = getValue s.registry val
runInstruction s (Set name val) = runStepValueInstruction s name val (\prev value -> value)
runInstruction s (Add name val) = runStepValueInstruction s name val (\prev value -> prev + value)
runInstruction s (Multiply name val) = runStepValueInstruction s name val (\prev value -> prev * value)
runInstruction s (Modulo name val) = runStepValueInstruction s name val (\prev value -> prev `mod` value)
runInstruction s (Recover name) =
  case Map.lookup name s.registry of
    Nothing -> s { position = s.position + 1 }
    Just n ->
      if n == (fromInt 0)
      then s { position = s.position + 1 }
      else case s.lastPlayedFrequency of
        Nothing -> s { position = s.position + 1 }
        Just n -> s { registry = Map.insert name n s.registry }
runInstruction s (Jump name1 name2) = if v1 > (fromInt 0) then s { position = s.position + v2' } else s { position = s.position + 1 }
  where v1 = fromMaybe (fromInt 0) $ getValue s.registry name1
        v2 = fromMaybe (fromInt 0) $ getValue s.registry name2
        v2' = round $ toNumber v2

findFrequency :: State -> Maybe BigInt
findFrequency initialState = tailRec go initialState
  where go state = case nextInstruction of
                      Nothing -> Done Nothing
                      Just instruction@(Recover name) -> case Map.lookup name state.registry of
                        Nothing -> Loop (runInstruction state instruction)
                        Just n ->
                          if n == fromInt 0
                          then Loop (runInstruction state instruction)
                          else Done state.lastPlayedFrequency
                      Just instruction -> Loop (runInstruction state instruction)
                   where nextInstruction = state.instructions !! state.position

makeInitialRegistry :: Input -> Int -> Registry
makeInitialRegistry instructions startingValue = Map.fromFoldable $ map (\name -> Tuple name (fromInt startingValue)) $ nub $ concatMap usedNames instructions
  where usedNames (Set n _) = [n]
        usedNames (Add n _) = [n]
        usedNames (Multiply n _) = [n]
        usedNames (Modulo n _) = [n]
        usedNames (Recover n) = [n]
        usedNames (Play v) = valueOrReferenceUsedNames v
        usedNames (Jump v1 v2) = valueOrReferenceUsedNames v1 <> valueOrReferenceUsedNames v2

valueOrReferenceUsedNames :: ValueOrReference -> Array Name
valueOrReferenceUsedNames (Value _) = []
valueOrReferenceUsedNames (Reference n) = [n]

solve :: Input -> Maybe BigInt
solve instructions = findFrequency initialState
  where initialState = {
          instructions: instructions,
          position: 0,
          registry: makeInitialRegistry instructions 0,
          lastPlayedFrequency: Nothing
        }


solve' = (map <<< map) solve (getInput "./src/18.txt") >>= logShow


----------

type DuetState = {
  instructions :: Array Instruction,
  position :: Int,
  registry :: Registry,
  incoming :: Array BigInt,
  outgoing :: Array BigInt
}

data RunningProgram = Zero | One
derive instance eqRunningProgram :: Eq RunningProgram
instance showRunningProgram :: Show RunningProgram where
  show Zero = "Zero"
  show One = "One"
other Zero = One
other One = Zero

type Duet = {
  running :: DuetState,
  waiting :: DuetState,
  active :: RunningProgram
}

data ExecutionState a = Waiting a | Running a

runInstructionDuet :: DuetState -> Instruction -> ExecutionState DuetState
runInstructionDuet s (Play val) = Running $ s { outgoing = snoc s.outgoing value, position = s.position + 1 }
  where value = unsafePartial $ fromJust $ getValue s.registry val
runInstructionDuet s (Set name val) = Running $ runStepValueInstruction s name val (\prev value -> value)
runInstructionDuet s (Add name val) = Running $ runStepValueInstruction s name val (\prev value -> prev + value)
runInstructionDuet s (Multiply name val) = Running $ runStepValueInstruction s name val (\prev value -> prev * value)
runInstructionDuet s (Modulo name val) = Running $ runStepValueInstruction s name val (\prev value -> prev `mod` value)
runInstructionDuet s (Recover name) = case uncons s.incoming of
  Nothing -> Waiting s
  Just {head: x, tail: xs} -> Running $ s { registry = Map.insert name x s.registry, incoming = xs, position = s.position + 1 }
runInstructionDuet s (Jump name1 name2) = if v1 > (fromInt 0) then Running $ s { position = s.position + v2' } else Running $ s { position = s.position + 1 }
  where v1 = fromMaybe (fromInt 0) $ getValue s.registry name1
        v2 = fromMaybe (fromInt 0) $ getValue s.registry name2
        v2' = round $ toNumber v2

runProgramUntilWaiting :: DuetState -> Maybe DuetState
runProgramUntilWaiting s = tailRec go s
  where go state = case nextInstruction of
                      Nothing -> Done Nothing
                      Just instruction -> case runInstructionDuet state instruction of
                        Running s -> Loop s
                        Waiting s -> Done (Just s)
                   where nextInstruction = state.instructions !! state.position


transferOutgoingToIncoming finished upNext =
  finished { outgoing = [] } /\ upNext { incoming = finished.outgoing }

runUntilDeadlock :: Duet -> Int
runUntilDeadlock duet = tailRec go initialState
  where initialState = {
          duet: duet,
          oneCount: 0
        }
        go state = case runProgramUntilWaiting state.duet.running of
          Nothing -> Done state.oneCount
          Just finishedRunning ->
            if null finishedRunning.outgoing
            then Done state.oneCount
            else let nextWaiting /\ nextRunning = transferOutgoingToIncoming finishedRunning state.duet.waiting
                     sentOnes = if state.duet.active == One then length finishedRunning.outgoing else 0
                 in Loop {
                       oneCount: state.oneCount + sentOnes,
                       duet: {
                         running: nextRunning,
                         waiting: nextWaiting,
                         active: other state.duet.active
                       }
                     }

makeInitialDuetState :: Array Instruction -> Int -> DuetState
makeInitialDuetState is v = {
  instructions: is,
  position: 0,
  registry: makeInitialRegistry is v,
  outgoing: [],
  incoming: []
}

solve2 instructions = runUntilDeadlock {running: running, waiting: waiting, active: Zero}
  where running = makeInitialDuetState instructions 0
        waiting = makeInitialDuetState instructions 1

solve2' = (map <<< map) solve2 (getInput "./src/18.txt") >>= logShow
