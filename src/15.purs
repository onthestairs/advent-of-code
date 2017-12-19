module Day15 where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (head, snoc, uncons)
import Data.BigInt (BigInt, fromInt, pow, toBase)
import Data.Foldable (length)
import Data.Int (binary, toStringAs)
import Data.List.Lazy as LazyList
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)

startA = fromInt 289
-- startA = fromInt 65
factorA = fromInt 16807
startB = fromInt 629
-- startB = fromInt 8921
factorB = fromInt 48271
modulo = fromInt 2147483647


rightMost = pow (fromInt 2) (fromInt 16)

lastBitsEqual :: BigInt -> BigInt -> Boolean
lastBitsEqual a b = (a `mod` rightMost) == (b `mod` rightMost)

fortyMillion = 40*1000*1000

solve :: Int
solve = tailRec go initialState
  where initialState = {a: startA, b: startB, equalCount: 0, n: 0}
        go state =
          if state.n > fortyMillion
          then Done state.equalCount
          else Loop {a: nextA, b: nextB, equalCount: nextEqualCount, n: state.n + 1}
          where nextA = (state.a * factorA) `mod` modulo
                nextB = (state.b * factorB) `mod` modulo
                areEqual = lastBitsEqual nextA nextB
                nextEqualCount = if areEqual then state.equalCount + 1 else state.equalCount

----

zero = fromInt 0
four = fromInt 4
eight = fromInt 8

aCriteria :: BigInt -> Boolean
aCriteria n = n `mod` four == zero

bCriteria :: BigInt -> Boolean
bCriteria n = n `mod` eight == zero

type State = {
  a :: BigInt,
  b :: BigInt,
  equalCount :: Int,
  judgeCount :: Int,
  asQueue :: Array BigInt,
  bsQueue :: Array BigInt
}

potentiallyJudge :: State -> State
potentiallyJudge state = case uncons state.asQueue of
  Nothing -> state
  Just { head: a, tail: as } -> case uncons state.bsQueue of
    Nothing -> state
    Just { head: b, tail: bs } ->
      if lastBitsEqual a b
      then state { asQueue = as, bsQueue = bs, judgeCount = state.judgeCount + 1, equalCount = state.equalCount + 1 }
      else state { asQueue = as, bsQueue = bs, judgeCount = state.judgeCount + 1 }

fiveMillion = 5*1000*1000
-- fiveMillion = 5

solve2 :: Int
solve2 = tailRec go initialState
  where initialState = {a: startA, b: startB, asQueue: [], bsQueue: [], judgeCount: 0, equalCount: 0}
        go state =
          if state.judgeCount == fiveMillion
          then Done state.equalCount
          else Loop nextState
          where nextA = (state.a * factorA) `mod` modulo
                nextB = (state.b * factorB) `mod` modulo
                isNextALegal = aCriteria nextA
                nextAsQueue = if isNextALegal then snoc state.asQueue nextA else state.asQueue
                isNextBLegal = bCriteria nextB
                nextBsQueue = if isNextBLegal then snoc state.bsQueue nextB else state.bsQueue
                nextState = potentiallyJudge {
                  a: nextA,
                  b: nextB,
                  equalCount: state.equalCount,
                  judgeCount: state.judgeCount,
                  asQueue: nextAsQueue,
                  bsQueue: nextBsQueue
                }
