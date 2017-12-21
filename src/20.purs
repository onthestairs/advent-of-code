module Day20 where

import Prelude

import Control.Fold (Fold, foldl, unfoldFold_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (concat, elem, filter, fromFoldable, group', head, mapWithIndex, range, sortBy)
import Data.Either (Either)
import Data.Foldable (length)
import Data.Int (toNumber)
import Data.String (trim)
import Data.Tuple (Tuple(..), snd)
import Data.Vector as V
import Data.Vector3 as V
import Day8 (parseSignedInt)
import Debug.Trace (trace)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (sepBy1)
import Text.Parsing.StringParser.String (char, string)

type Vector = V.Vec3 Number
newtype Particle = Particle {
  position :: Vector,
  velocity :: Vector,
  acceleration :: Vector
}
instance showParticle :: Show Particle where
  show (Particle {position, velocity, acceleration}) = "P: " <> show position <> ", V: " <> show velocity <> ", A: " <> show acceleration
getPosition (Particle {position, velocity, acceleration}) = position
type Input = Array Particle

parseVector :: Parser Vector
parseVector = do
  _ <- char '<'
  x <- parseSignedInt
  _ <- char ','
  y <- parseSignedInt
  _ <- char ','
  z <- parseSignedInt
  _ <- char '>'
  pure (V.fromArray [toNumber x, toNumber y, toNumber z])

parseParticle :: Parser Particle
parseParticle = do
  _ <- string "p="
  position <- parseVector
  _ <- string ", "
  _ <- string "v="
  velocity <- parseVector
  _ <- string ", "
  _ <- string "a="
  acceleration <- parseVector
  pure $ Particle {position: position, velocity: velocity, acceleration: acceleration}

parseString :: String -> Either ParseError Input
parseString = runParser $ fromFoldable <$> (sepBy1 parseParticle (char '\n'))

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

solve particles = head $ sortBy (comparing snd) $ mapWithIndex (\i p -> Tuple i (accelerationMagnitude p)) particles
  where accelerationMagnitude (Particle {position, velocity, acceleration}) = V.vlength acceleration

solve' = (map <<< map) solve (getInput "./src/20.txt") >>= logShow


----------
tick :: Particle -> Particle
tick (Particle {position, velocity, acceleration}) = Particle {
  position: position + velocity + acceleration,
  velocity: velocity + acceleration,
  acceleration: acceleration
}

-- removeDuplicatesBy :: forall a b. Ord b => (a -> b) -> Array a -> Array a
-- removeDuplicatesBy f xs = filter (\x -> f x `elem` vs) xs
--   where vs = concat $ map fromFoldable $ filter (\g -> length g > 2) $ group' $ map f xs

removeDuplicatesBy :: forall a b. Eq b => (a -> b) -> Array a -> Array a
removeDuplicatesBy f xs = filter (\x -> f x `elem` nonDuplicates) xs
  where vs = map f xs
        nonDuplicates = filter (\v -> length (filter ((==) v) vs) == 1) vs

stepAndPrune :: forall a. Array Particle -> Fold a (Array Particle)
stepAndPrune startingParticles = unfoldFold_ startingParticles (\ps i -> removeDuplicatesBy getPosition (map tick ps))

solve2 :: Input -> Int
solve2 particles = length $ foldl (stepAndPrune particles) (range 0 (1*1000))

solve2' = (map <<< map) solve2 (getInput "./src/20.txt") >>= logShow
