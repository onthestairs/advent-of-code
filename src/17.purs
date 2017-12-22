module Day17 where

import Prelude

import Control.Fold (Fold, foldl, scanl, unfoldFold_)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (findIndex, insertAt, range, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))


type CircularBuffer a = {
  buffer :: Array a,
  length :: Int,
  position :: Int
}

step :: forall a. CircularBuffer a -> Int -> CircularBuffer a
step b n = b { position = newPosition }
  where newPosition = (b.position + n) `mod` b.length

insert :: forall a. CircularBuffer a -> a -> CircularBuffer a
insert b x = step newCircularBuffer 1
  where newBuffer = fromMaybe b.buffer $ insertAt (b.position + 1) x b.buffer
        newLength = b.length + 1
        newCircularBuffer = b { buffer = newBuffer, length = newLength }

getValue :: forall a. CircularBuffer a -> Maybe a
getValue b = b.buffer !! b.position

emptyBuffer = {
  buffer: [0],
  length: 1,
  position: 0
}

insertAndMove :: Int -> Fold Int (CircularBuffer Int)
insertAndMove delta = unfoldFold_ emptyBuffer f
  where f b n = insert (step b delta) n

solve = getValue $ flip step 1 $ foldl (insertAndMove 382) (range 1 2017)


------------

stepToValue :: forall a. Ord a => CircularBuffer a -> a -> CircularBuffer a
stepToValue b x = b {position = i}
  where i = fromMaybe b.position $ findIndex ((==) x) b.buffer


findValueAfterZero step limit = tailRec go initialState
  where initialState = {valueAfterZero: Nothing, valueToPlace: 1, currentPosition: 0}
        go state =
          if state.valueToPlace > limit
          then Done state.valueAfterZero
          else Loop {valueAfterZero: nextValueAfterZero, valueToPlace: state.valueToPlace + 1, currentPosition: nextPosition}
            where nextPosition = ((state.currentPosition + step) `mod` state.valueToPlace) + 1
                  nextValueAfterZero = if nextPosition == 1 then Just state.valueToPlace else state.valueAfterZero

fiftyMillion = 50*1000*1000
solve2 = findValueAfterZero 382 fiftyMillion
