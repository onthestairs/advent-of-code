module Day1 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (drop, filter, head, length, span, tail, take, zip)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (singleton, toCharArray, trim)
import Data.Tuple (Tuple(..), fst)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)

type Input = Array Int

parseString :: String -> Input
parseString = (map (fromMaybe 0 <<< fromString <<< singleton) <<< toCharArray) <<< trim

getInput :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) Input
getInput filename =
  map parseString (readTextFile UTF8 filename)

shift :: forall a. Eq a => Array a -> Array a
shift xs = case head xs of
  Nothing -> xs
  Just x -> let {init, rest} = span ((==) x) xs in rest <> init

-- Note that this is broken on lists with only one unique value
solve :: Input -> Int
solve xs = sum $ map fst $ filter (\(Tuple y1 y2) -> y1 == y2) (zip ys (fromMaybe [] (tail ys)))
  where ys = shift xs

solve' :: forall eff. Eff (console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff) Unit
solve' = map solve (getInput "./src/1.txt") >>= logShow

----
shiftN :: forall a. Int -> Array a -> Array a
shiftN n xs = drop n xs <> take n xs

solve2 :: Input -> Int
solve2 xs = sum $ map fst $ filter (\(Tuple y1 y2) -> y1 == y2) (zip xs (shiftN (l/2) xs))
  where l = length xs

-- this is simpler and actually correct (doesn't have the defect from above :D)
solve2' :: forall eff. Eff (console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff) Unit
solve2' = map solve2 (getInput "./src/1.txt") >>= logShow
