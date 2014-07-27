{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main as Criterion
import Data.List (intersect)
import qualified Data.Set as Set
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck


listIntersect1 :: (Ord a) => [a] -> [a] -> [a]
listIntersect1 a b = filter (`Set.member` bSet) a
  where
    bSet = Set.fromList b


tests :: IO ()
tests = hspec $ describe "QuickCheck properties" $ do
  isOK listIntersect1 "listIntersect1"
  where
    isOK f name = prop (name ++ " == intersect") $ \(a, b :: [Int]) ->
                    counterexample (show (a, b)) $
                      a `f` b === a `intersect` b


main :: IO ()
main = Criterion.defaultMain
  [ bgroup "simple"
    [ bench "[1..1000] `intersect` [200..700]"       $ nf ([1..1000] `intersect`) [200..700::Int]
    , bench "[1..2000] `intersect` [400..1400]"      $ nf ([1..2000] `intersect`) [400..1400::Int]
    ]

  , bgroup "listIntersect1"
    [ bench "[1..1000] `listIntersect1` [200..700]"          $ nf ([1..1000] `listIntersect1`) [200..700::Int]
    , bench "[1..2000] `listIntersect1` [400..1400]"         $ nf ([1..2000] `listIntersect1`) [400..1400::Int]
    , bench "replicate 16000 1 `listIntersect1` [400..1400]" $ nf (replicate 16000 1 `listIntersect1`) [400..1400::Int]
    ]
  ]
