module Day8 where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (length, maximum)
import Data.Generic (class Generic, gShow)
import Data.List (List, catMaybes, foldl, nub, scanl, zip)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.StrMap (StrMap, alter, fromFoldable, lookup, values)
import Data.String (fromCharArray, trim)
import Data.Unfoldable (replicate)
import Day2 (parseInt)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (many1, optionMaybe, sepBy1)
import Text.Parsing.StringParser.String (char, lowerCaseChar, string)

type Instructions = List Instruction
type Register = String

data Comparitor = GT | GTE | LTE | LT | EQ | NE
derive instance genericComparitor :: Generic Comparitor
instance showComparitor :: Show Comparitor where
  show = gShow

data Condition = Condition Register Comparitor Int
derive instance genericCondition :: Generic Condition
instance showCondition :: Show Condition where
  show = gShow

data Action = Inc Int | Dec Int
derive instance genericAction :: Generic Action
instance showAction :: Show Action where
  show = gShow

data Instruction = Instruction Register Action Condition
derive instance genericInstruction :: Generic Instruction
instance showInstruction :: Show Instruction where
  show = gShow

parseRegister :: Parser Register
parseRegister = (fromCharArray <<< Array.fromFoldable) <$> many1 lowerCaseChar

parseSignedInt :: Parser Int
parseSignedInt = do
 neg <- optionMaybe (char '-')
 let multiplier = maybe 1 (const (-1)) neg
 int <- parseInt
 pure (multiplier * int)

parseAction :: Parser Action
parseAction = do
  f <- (string "dec" $> Dec) <|> (string "inc" $> Inc)
  _ <- char ' '
  n <- parseSignedInt
  pure $ f n

parseComparitor :: Parser Comparitor
parseComparitor =
  (string "<=" $> LTE) <|>
  (string ">=" $> GTE) <|>
  (string "<" $> LT) <|>
  (string ">" $> GT) <|>
  (string "==" $> EQ) <|>
  (string "!=" $> NE)

parseCondition = do
  _ <- string "if"
  _ <- char ' '
  register <- parseRegister
  _ <- char ' '
  comparitor <- parseComparitor
  _ <- char ' '
  n <- parseSignedInt
  pure $ Condition register comparitor n

parseInstruction :: Parser Instruction
parseInstruction = do
  register <- parseRegister
  _ <- char ' '
  action <- parseAction
  _ <- char ' '
  condition <- parseCondition
  pure $ Instruction register action condition

parseInstructions :: Parser Instructions
parseInstructions = sepBy1 parseInstruction (char '\n')

parseString :: String -> Either ParseError Instructions
parseString input = runParser parseInstructions input

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Instructions)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)


---------
type Registry = StrMap Int

comparitorToFunction :: Comparitor -> (Int -> Int -> Boolean)
comparitorToFunction GT = (>)
comparitorToFunction GTE = (>=)
comparitorToFunction LT = (<)
comparitorToFunction LTE = (<=)
comparitorToFunction EQ = (==)
comparitorToFunction NE = (/=)

testCondition :: Registry -> Condition -> Boolean
testCondition m (Condition register comparitor n) =
  fromMaybe false $ (\v' -> c v' n) <$> v
  where v = lookup register m
        c = comparitorToFunction comparitor

runAction :: Registry -> Register -> Action -> Registry
runAction m register a = alter (liftA1 f) register m
  where f = case a of
              (Dec n) -> (\m -> m - n)
              (Inc n) -> (\m -> m + n)

runInstruction :: Registry -> Instruction -> Registry
runInstruction m (Instruction register action condition) =
  if testCondition m condition then runAction m register action else m

runInstructions :: Registry -> List Instruction -> Registry
runInstructions m instructions =
  foldl runInstruction m instructions

findUsedRegisters :: Instructions -> List Register
findUsedRegisters is = nub $ (toModify <> toCompare)
  where toModify = map (\(Instruction register _ _) -> register) is
        toCompare = map (\(Instruction _ _ (Condition register _ _)) -> register) is

largestRegisterValue :: Registry -> Maybe Int
largestRegisterValue m = maximum $ values m

solve :: Instructions -> Maybe Int
solve instructions = largestRegisterValue $ (runInstructions m instructions)
  where usedRegisters = findUsedRegisters instructions
        m = fromFoldable $ zip usedRegisters (replicate (length usedRegisters) 0)

solve' = (map <<< map) solve (getInput "./src/8.txt") >>= logShow

-----

runInstructions2 :: Registry -> List Instruction -> List Registry
runInstructions2 m instructions =
  scanl runInstruction m instructions

solve2 :: Instructions -> Maybe Int
solve2 instructions = maximum $ catMaybes $ map largestRegisterValue $ (runInstructions2 m instructions)
  where usedRegisters = findUsedRegisters instructions
        m = fromFoldable $ zip usedRegisters (replicate (length usedRegisters) 0)

solve2' = (map <<< map) solve2 (getInput "./src/8.txt") >>= logShow
