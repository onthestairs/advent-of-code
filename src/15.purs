module Day15 where

import Prelude

import Data.Foldable (length)
import Data.Int (binary, toStringAs)
import Data.List.Lazy as LazyList
import Data.String as String
import Data.Tuple.Nested ((/\))

startA = 289
factorA = 16807
startB = 629
factorB = 48271
modulo = 2147483647

type Start = Int
type Factor = Int

generate :: Start -> Factor -> LazyList.List Int
generate n f = LazyList.iterate g n
  where g m = (m * f) `mod` modulo

lastNBinaryEqual n a b = aBinLast == bBinLast
  where aBin = toStringAs binary a
        aBinLast = String.drop ((String.length aBin) - n) aBin
        bBin = toStringAs binary b
        bBinLast = String.drop ((String.length aBin) - n) aBin

numberOfMatches :: Int -> Int
numberOfMatches repetitions = length $ LazyList.filter isEqual (LazyList.take repetitions pairs)
  where as = generate startA factorA
        bs = generate startB factorB
        pairs = LazyList.zip as bs
        isEqual (a /\ b) = lastNBinaryEqual 16 a b

solve :: Int
solve = numberOfMatches (40*1000*1000)
