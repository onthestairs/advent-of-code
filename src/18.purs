module Day18 where

import Prelude

import Control.Alt ((<|>))
import Control.Fold (scanl)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION, name)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (concatMap, fromFoldable, length, nub, (!!))
import Data.Either (Either)
import Data.Generic (class Generic, gShow)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Day8 (parseSignedInt)
import Debug.Trace (spy)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
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

type RegistryState = Int
type Registry = Map.Map Name RegistryState

type State = {
  instructions :: Array Instruction,
  position :: Int,
  registry :: Registry,
  lastPlayedFrequency :: Maybe Int
}

showState :: State -> String
showState s = show m <> " " <> "(" <> show currentInstruction <> ")" <> " " <> show s.position
  where currentInstruction = s.instructions !! s.position
        m = map (\(Tuple k v) -> show k <> ":" <> show v) $ (Map.toUnfoldable s.registry :: Array (Tuple Name RegistryState))

getValue :: Registry -> ValueOrReference -> Maybe Int
getValue r (Value n) = Just n
getValue r (Reference name) = Map.lookup name r

modifyName :: Name -> (RegistryState -> RegistryState) -> Registry -> Registry
modifyName name f r = Map.update (Just <<< f) name r

runStepValueInstruction :: State -> Name -> ValueOrReference -> (Int -> Int -> Int) -> State
runStepValueInstruction s name val f = s {registry = newRegistry, position = s.position + 1}
  where value = getValue s.registry val
        newRegistry = case value of
          Nothing -> s.registry
          Just v -> modifyName name (\prev -> f prev v) s.registry

runInstruction :: State -> Instruction -> State
runInstruction s (Play val) = s { lastPlayedFrequency = spy value, position = s.position + 1 }
  where value = getValue s.registry val
runInstruction s (Set name val) = runStepValueInstruction s name val (\prev value -> value)
runInstruction s (Add name val) = runStepValueInstruction s name val (\prev value -> prev + value)
runInstruction s (Multiply name val) = runStepValueInstruction s name val (\prev value -> prev * value)
runInstruction s (Modulo name val) = runStepValueInstruction s name val (\prev value -> prev `mod` value)
runInstruction s (Recover name) =
  case Map.lookup name s.registry of
    Nothing -> s { position = s.position + 1 }
    Just 0 -> s { position = s.position + 1 }
    otherwise -> case s.lastPlayedFrequency of
      Nothing -> s { position = s.position + 1 }
      Just n -> s { registry = Map.insert name n s.registry }
runInstruction s (Jump name1 name2) = if v1 > 0 then s { position = s.position + v2 } else s { position = s.position + 1 }
  where v1 = fromMaybe 0 $ getValue s.registry name1
        v2 = fromMaybe 0 $ getValue s.registry name2

findFrequency :: State -> Maybe Int
findFrequency initialState = tailRec go initialState
  where go state = case nextInstruction of
                      Nothing -> Done Nothing
                      Just instruction@(Recover name) -> case Map.lookup name state.registry of
                        Nothing -> Loop (runInstruction state instruction)
                        Just 0 -> Loop (runInstruction state instruction)
                        Just n -> Done state.lastPlayedFrequency
                      Just instruction -> Loop (runInstruction state instruction)
                   where nextInstruction = state.instructions !! state.position
                         blah = spy (showState state)
        -- blah1 = spy (Map.keys initialState.registry)

makeInitialRegistry :: Input -> Registry
makeInitialRegistry instructions = Map.fromFoldable $ map (\name -> Tuple name 0) $ nub $ concatMap usedNames instructions
  where usedNames (Set n _) = [n]
        usedNames (Add n _) = [n]
        usedNames (Multiply n _) = [n]
        usedNames (Modulo n _) = [n]
        usedNames (Recover n) = [n]
        usedNames _ = []

solve :: Input -> Maybe Int
solve instructions = findFrequency initialState
  where initialState = {
          instructions: instructions,
          position: 0,
          registry: makeInitialRegistry instructions,
          lastPlayedFrequency: Nothing
        }


solve' = (map <<< map) solve (getInput "./src/18.txt") >>= logShow
