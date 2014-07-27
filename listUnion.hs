{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main as Criterion
import Data.List (union)
import qualified Data.Set as Set
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck


ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ []     = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs


listUnion1 :: (Ord a) => [a] -> [a] -> [a]
listUnion1 a b = a ++ filter (`Set.notMember` aSet) (ordNub b)
  where
    aSet = Set.fromList a


listUnion2 :: (Ord a) => [a] -> [a] -> [a]
listUnion2 a b = a ++ ordNub (filter (`Set.notMember` aSet) b)
  where
    aSet = Set.fromList a


tests :: IO ()
tests = hspec $ describe "QuickCheck properties" $ do
  isOK listUnion1 "listUnion1"
  isOK listUnion2 "listUnion2"
  where
    isOK f name = prop (name ++ " == union") $ \(a, b :: [Int]) ->
                    counterexample (show (a, b)) $
                      a `f` b === a `union` b


main :: IO ()
main = Criterion.defaultMain
  [ bgroup "simple"
    [ bench "[1..1000] `union` [200..700]"       $ nf ([1..1000] `union`) [200..700::Int]
    , bench "[1..2000] `union` [400..1400]"      $ nf ([1..2000] `union`) [400..1400::Int]
    ]

  , bgroup "listUnion1"
    [ bench "[1..1000] `listUnion1` [200..700]"          $ nf ([1..1000] `listUnion1`) [200..700::Int]
    , bench "[1..2000] `listUnion1` [400..1400]"         $ nf ([1..2000] `listUnion1`) [400..1400::Int]
    , bench "replicate 16000 1 `listUnion1` [400..1400]" $ nf (replicate 16000 1 `listUnion1`) [400..1400::Int]
    ]

  , bgroup "listUnion2"
    [ bench "[1..1000] `listUnion2` [200..700]"          $ nf ([1..1000] `listUnion2`) [200..700::Int]
    , bench "[1..2000] `listUnion2` [400..1400]"         $ nf ([1..2000] `listUnion2`) [400..1400::Int]
    , bench "replicate 16000 1 `listUnion2` [400..1400]" $ nf (replicate 16000 1 `listUnion2`) [400..1400::Int]
    ]
  ]
