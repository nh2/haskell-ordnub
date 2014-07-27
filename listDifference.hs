{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main as Criterion
import Data.List ((\\), mapAccumL)
import Data.Maybe
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck


listDifference1 :: (Ord a) => [a] -> [a] -> [a]
listDifference1 a b = catMaybes . snd $ mapAccumL f initHist a
  where
    initHist = Map.fromListWith (+) [ (x, 1 :: Int) | x <- b ]
    f hist x = case Map.lookup x hist of
      Just n | n > 0 -> (Map.insert x (n-1) hist, Nothing)
      _              -> (hist                   , Just x)


listDifference2 :: (Ord a) => [a] -> [a] -> [a]
listDifference2 a b = catMaybes . snd $ mapAccumL f initHist a
  where
    initHist = Map.fromListWith (+) [ (x, 1 :: Int) | x <- b ]
    decrease  1 = Nothing
    decrease !n = Just (n - 1)
    f hist x = case Map.updateLookupWithKey (const decrease) x hist of
      (Just _, hist') -> (hist', Nothing)
      _               -> (hist , Just x)


listDifference3 :: (Ord a) => [a] -> [a] -> [a]
listDifference3 a b = go initHist a
  where
    initHist = Map.fromListWith (+) [ (x, 1 :: Int) | x <- b ]

    go _    []     = []
    go hist (x:xs) = case Map.lookup x hist of
      Just n | n > 0 ->     go (Map.insert x (n-1) hist) xs
      _              -> x : go hist                      xs


listDifference4 :: (Ord a) => [a] -> [a] -> [a]
listDifference4 a b = let initHist = Map.fromListWith (+) [ (x, 1 :: Int) | x <- b ]
               in  go initHist a
  where
    decrease  1 = Nothing
    decrease !n = Just (n - 1)

    go _    []     = []
    go hist (x:xs) = case Map.updateLookupWithKey (const decrease) x hist of
      (Just _, hist') ->     go hist' xs
      _               -> x : go hist                      xs


tests :: IO ()
tests = hspec $ describe "QuickCheck properties" $ do
  isOK listDifference1 "listDifference1"
  isOK listDifference2 "listDifference2"
  isOK listDifference3 "listDifference3"
  isOK listDifference4 "listDifference4"
  where
    isOK f name = prop (name ++ " == (\\\\)") $
                    \(a, b :: [Int]) -> a `f` b === a \\ b


main :: IO ()
main = Criterion.defaultMain
  [ bgroup "simple"
    [ bench "[1..1000] \\\\ [200..700]"       $ nf ([1..1000] \\) [200..700::Int]
    , bench "[1..2000] \\\\ [400..1400]"      $ nf ([1..2000] \\) [400..1400::Int]
    ]

  , bgroup "listDifference1"
    [ bench "[1..1000] `listDifference1` [200..700]"          $ nf ([1..1000] `listDifference1`) [200..700::Int]
    , bench "[1..2000] `listDifference1` [400..1400]"         $ nf ([1..2000] `listDifference1`) [400..1400::Int]
    , bench "replicate 16000 1 `listDifference1` [400..1400]" $ nf (replicate 16000 1 `listDifference1`) [400..1400::Int]
    ]

  , bgroup "listDifference2"
    [ bench "[1..1000] `listDifference2` [200..700]"          $ nf ([1..1000] `listDifference2`) [200..700::Int]
    , bench "[1..2000] `listDifference2` [400..1400]"         $ nf ([1..2000] `listDifference2`) [400..1400::Int]
    , bench "replicate 16000 1 `listDifference2` [400..1400]" $ nf (replicate 16000 1 `listDifference2`) [400..1400::Int]
    ]

  , bgroup "listDifference3"
    [ bench "[1..1000] `listDifference3` [200..700]"          $ nf ([1..1000] `listDifference3`) [200..700::Int]
    , bench "[1..2000] `listDifference3` [400..1400]"         $ nf ([1..2000] `listDifference3`) [400..1400::Int]
    , bench "replicate 16000 1 `listDifference3` [400..1400]" $ nf (replicate 16000 1 `listDifference3`) [400..1400::Int]
    ]

  , bgroup "listDifference4"
    [ bench "[1..1000] `listDifference4` [200..700]"          $ nf ([1..1000] `listDifference4`) [200..700::Int]
    , bench "[1..2000] `listDifference4` [400..1400]"         $ nf ([1..2000] `listDifference4`) [400..1400::Int]
    , bench "replicate 16000 1 `listDifference4` [400..1400]" $ nf (replicate 16000 1 `listDifference4`) [400..1400::Int]
    ]
  ]
