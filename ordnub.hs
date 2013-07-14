module Main where

import Control.Monad.State.Strict
import Data.Int (Int64)
import Data.List (nub)
import Data.Set (empty, fromList, insert, member)

import Test.QuickCheck
import Criterion.Main


-- Just copied from Data.List
localNub :: (Eq a) => [a] -> [a]
localNub l            = nub' l []
  where
    nub' [] _         = []
    nub' (x:xs) ls
        | x `elem` ls = nub' xs ls
        | otherwise   = x : nub' xs (x:ls)


ordNub :: (Eq a, Ord a) => [a] -> [a]
ordNub l = go l empty id
  where
    go []     _ dlist = dlist []
    go (x:xs) s dlist = if member x s
                          then go xs s dlist
                          else go xs (insert x s) (dlist . (x:))


-- Using a state monad
ordNubState :: (Eq a, Ord a) => [a] -> [a]
ordNubState xs = evalState (filterM f xs) empty
  where
    f x = do set <- get
             if member x set
               then return False
               else put (insert x set) >> return True


main :: IO ()
main = defaultMain
  [ bench "benchmarks:" $ nf id 'x' -- just so that I can comment out easily

  -- , bench "1000 nub" $ nf nub l1000
  -- , bench "500  nub" $ nf nub l500
  , bench "100  nub" $ nf nub l100
  , bench "50   nub" $ nf nub l50
  , bench "10   nub" $ nf nub l10
  , bench "5    nub" $ nf nub l5
  , bench "1    nub" $ nf nub l1

  -- , bench "1000 localNub" $ nf localNub l1000
  -- , bench "500  localNub" $ nf localNub l500
  , bench "100  localNub" $ nf localNub l100
  , bench "50   localNub" $ nf localNub l50
  , bench "10   localNub" $ nf localNub l10
  , bench "5    localNub" $ nf localNub l5
  , bench "1    localNub" $ nf localNub l1

  -- , bench "1000 ordNub" $ nf ordNub l1000
  -- , bench "500  ordNub" $ nf ordNub l500
  , bench "100  ordNub" $ nf ordNub l100
  , bench "50   ordNub" $ nf ordNub l50
  , bench "10   ordNub" $ nf ordNub l10
  , bench "5    ordNub" $ nf ordNub l5
  , bench "1    ordNub" $ nf ordNub l1

  -- , bench "1000 ordNubState" $ nf ordNubState l1000
  -- , bench "500  ordNubState" $ nf ordNubState l500
  , bench "100  ordNubState" $ nf ordNubState l100
  , bench "50   ordNubState" $ nf ordNubState l50
  , bench "10   ordNubState" $ nf ordNubState l10
  , bench "5    ordNubState" $ nf ordNubState l5
  , bench "1    ordNubState" $ nf ordNubState l1
  ]
  where
    -- l1000 = concat $ replicbate 10 [1..1000::Int]
    -- l500  = concat $ replicate 20  [1..500::Int]
    l100  = concat $ replicate 100 [1..100::Int]
    l50   = concat $ replicate 200  [1..50::Int]
    l10   = concat $ replicate 1000 [1..10::Int]
    l5    = concat $ replicate 2000  [1..5::Int]
    l1    = concat $ replicate 10000    [1::Int]


tests :: IO ()
tests = mapM_ quickCheck [ isLikeNub localNub
                         , isLikeNub ordNub
                         , isLikeNub ordNubState
                         ]
  where
    isLikeNub f = property (\l -> nub l == f (l :: [Int]))
