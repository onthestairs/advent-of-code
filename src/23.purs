module Day23 where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (intercalate, (!!))
import Data.Array as Array
import Data.BigInt (BigInt, fromInt, toNumber)
import Data.Either (Either)
import Data.Generic (class Generic, gShow)
import Data.Int (round)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (singleton, toCharArray, trim)
import Data.Tuple (Tuple(..))
import Day8 (parseSignedInt)
import Debug.Trace (trace, traceShow)
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
  show (Reference name) = "*" <> singleton name
data Instruction
  = Set Name ValueOrReference
  | Sub Name ValueOrReference
  | Multiply Name ValueOrReference
  | Jump ValueOrReference ValueOrReference
-- derive instance genericInstruction :: Generic Instruction
-- instance showInstruction :: Show Instruction
--   where show = gShow
instance showInstruction :: Show Instruction
  where show (Set name value) = "set " <> singleton name <> " " <> show value
        show (Sub name value) = "sub " <> singleton name <> " " <> show value
        show (Multiply name value) = "mul " <> singleton name <> " " <> show value
        show (Jump name value) = "jnz " <> show name <> " " <> show value
type Input = Array Instruction

parseName :: Parser Name
parseName = lowerCaseChar

parseValueOrReference :: Parser ValueOrReference
parseValueOrReference =
  (Value <$> parseSignedInt) <|>
  (Reference <$> parseName)

parseSet :: Parser Instruction
parseSet = do
  _ <- string "set"
  skipSpaces
  name <- parseName
  skipSpaces
  val <- parseValueOrReference
  pure (Set name val)

parseSub :: Parser Instruction
parseSub = do
  _ <- string "sub"
  skipSpaces
  name <- parseName
  skipSpaces
  val <- parseValueOrReference
  pure (Sub name val)

parseMultiply :: Parser Instruction
parseMultiply = do
  _ <- string "mul"
  skipSpaces
  name <- parseName
  skipSpaces
  val <- parseValueOrReference
  pure (Multiply name val)

parseJump :: Parser Instruction
parseJump = do
  _ <- string "jnz"
  skipSpaces
  name <- parseValueOrReference
  skipSpaces
  val <- parseValueOrReference
  pure (Jump name val)

parseInstruction :: Parser Instruction
parseInstruction =
  parseSet <|>
  parseSub <|>
  parseMultiply <|>
  parseJump

parseString :: String -> Either ParseError Input
parseString = runParser $ Array.fromFoldable <$> (sepBy1 parseInstruction (char '\n'))

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

---

type RegistryState = BigInt
type Registry = Map.Map Name RegistryState
showRegistry :: Registry -> Array String
showRegistry r = map (\(Tuple c n) -> singleton c <> ":" <> show (toNumber n)) $ Map.toUnfoldable r

type State = {
  instructions :: Array Instruction,
  position :: Int,
  registry :: Registry,
  instructionsRead :: Int
}

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
runInstruction s (Set name val) = runStepValueInstruction s name val (\prev value -> value)
runInstruction s (Sub name val) = runStepValueInstruction s name val (\prev value -> prev - value)
runInstruction s (Multiply name val) = runStepValueInstruction s name val (\prev value -> prev * value)
runInstruction s (Jump name1 name2) = if v1 /= (fromInt 0) then s { position = s.position + v2' } else s { position = s.position + 1 }
  where v1 = unsafePartial $ fromJust $ getValue s.registry name1
        v2 = unsafePartial $ fromJust $ getValue s.registry name2
        v2' = round $ toNumber v2

runWithAuxState :: forall a. State -> a -> (Instruction -> State -> a -> a) -> a
runWithAuxState initialState initialAuxState f = tailRec go {instructionsState: initialState, auxState: initialAuxState}
  where go state = if state.instructionsState.instructionsRead == 1000 then Done state.auxState else case nextInstruction of
                      Nothing -> Done state.auxState
                      Just instruction -> let nextInstructionsState = runInstruction state.instructionsState (trace (show instruction <> " - " <> show (showRegistry state.instructionsState.registry)) \_ -> instruction)
                                              nextAuxState = f instruction nextInstructionsState state.auxState
                                          in Loop {
                                            instructionsState: nextInstructionsState {instructionsRead = nextInstructionsState.instructionsRead + 1},
                                            auxState: nextAuxState
                                          }
                   where nextInstruction = state.instructionsState.instructions !! state.instructionsState.position

makeInitialRegistry f = Map.fromFoldable $ map (\c -> Tuple c (f c)) $ toCharArray "abcdefgh"

solve :: Input -> Int
solve instructions = runWithAuxState initialState initialAuxState f
  where initialState = {
          instructions: instructions,
          position: 0,
          registry: makeInitialRegistry (const (fromInt 0)),
          instructionsRead: 0
        }
        initialAuxState = 0
        f (Multiply _ _) _ n = n + 1
        f _ _ n = n

solve' = (map <<< map) solve (getInput "./src/23.txt") >>= logShow

-----

solve2 :: Input -> Maybe BigInt
solve2 instructions = runWithAuxState initialState initialAuxState f
  where initialState = {
          instructions: instructions,
          position: 0,
          registry: makeInitialRegistry (\c -> if c == 'a' then fromInt 1 else fromInt 0),
          instructionsRead: 0
        }
        initialAuxState = Just (fromInt 0)
        f _ s _ = Map.lookup 'h' s.registry

solve2' = (map <<< map) solve2 (getInput "./src/23.txt") >>= logShow
