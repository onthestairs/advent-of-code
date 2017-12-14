module Day12 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Either (Either)
import Data.Foldable (elem, length)
import Data.Graph as Graph
import Data.List (List(..), concat, concatMap, difference, foldl, nub, singleton, (:))
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (trim)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Day8 (parseSignedInt)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (sepBy1)
import Text.Parsing.StringParser.String (char, string)

type Node = Int
type Input = List (Tuple Node (List Node))

parseLine :: Parser (Tuple Node (List Node))
parseLine = do
  node <- parseSignedInt
  _ <- char ' '
  _ <- string "<->"
  _ <- char ' '
  nodes <- sepBy1 parseSignedInt (string ", ")
  pure (Tuple node nodes)

parseString :: String -> Either ParseError Input
parseString = runParser $ sepBy1 parseLine (char '\n')

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError Input)
getInput filename =
  map (parseString <<< trim) (readTextFile UTF8 filename)

---

makeGraph :: Input -> Graph.Graph Int Int
makeGraph xs = Graph.fromMap $ Map.fromFoldable $ map (\(n /\ ns) -> n /\ (n /\ ns)) xs

solve :: Input -> Int
solve xs = length ns
  where g = makeGraph xs
        ns = recursiveOutNodes 0 g

recursiveOutNodes :: forall k v. Ord k => k -> Graph.Graph k v -> List k
recursiveOutNodes k g = nub $ tailRec go {ns: Nil, queue: singleton k}
  where go {ns: ns, queue: Nil} = Done ns
        go {ns: ns, queue: Cons l ls} = Loop { ns: ns <> new, queue: ls <> new}
          where next = fromMaybe Nil $ Graph.outEdges l g
                new = difference next ns

solve' = (map <<< map) solve (getInput "./src/12.txt") >>= logShow


------

connectedRegions :: forall k. Ord k => Graph.Graph k k -> Int
connectedRegions g = length $ foldl (\ms n -> go n ms) Nil ns
  where ns = Graph.vertices g
        go n ms = if (elem n $ concat ms) then ms else ((recursiveOutNodes n g) : ms)

solve2 :: Input -> Int
solve2 xs = length $ foldl (\ms n -> go n ms) Nil ns
  where g :: Graph.Graph Int Int
        g = makeGraph xs
        ns :: List Int
        ns = map fst xs
        go :: Int -> List (List Int) -> List (List Int)
        go n ms = if (elem n $ concat ms) then ms else ((recursiveOutNodes n g) : ms)


solve2' = (map <<< map) solve2 (getInput "./src/12.txt") >>= logShow
