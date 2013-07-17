module Main where

import Control.Monad.State.Strict
import Data.Int (Int64)
import Data.List (nub)
import qualified Data.Set as Set

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


-- Taken From Yi
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ []     = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs


-- Using a state monad
ordNubState :: (Ord a) => [a] -> [a]
ordNubState xs = evalState (filterM f xs) Set.empty
  where
    f x = do set <- get
             if Set.member x set
               then return False
               else put (Set.insert x set) >> return True


-- Using a state monad with a dlist instead of filterM
ordNubStateDlist :: (Ord a) => [a] -> [a]
ordNubStateDlist l = evalState (f l id) Set.empty
  where
    f []     dlist = return $ dlist []
    f (x:xs) dlist = do set <- get
                        if Set.member x set
                          then f xs dlist
                          else put (Set.insert x set) >> f xs (dlist . (x:))


main :: IO ()
main = defaultMain
  [ bgroup "simple"
    [ bench "nub [1..100]"  $ nf nub [1..100::Int]
    , bench "nub [1..1000]" $ nf nub [1..1000::Int]
    , bench "nub (replicate 1000 1)" $ nf nub (replicate 1000 (1::Int))

    , bench "ordNub [1..100]"  $ nf ordNub [1..100::Int]
    , bench "ordNub [1..1000]" $ nf ordNub [1..1000::Int]
    , bench "ordNub (replicate 1000 1)" $ nf ordNub (replicate 1000 (1::Int))
    ]

  , bgroup ""
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

    -- -- , bench "1000 ordNub" $ nf ordNub l1000
    -- -- , bench "500  ordNub" $ nf ordNub l500
    , bench "100  ordNub" $ nf ordNub l100
    , bench "50   ordNub" $ nf ordNub l50
    , bench "10   ordNub" $ nf ordNub l10
    , bench "5    ordNub" $ nf ordNub l5
    , bench "1    ordNub" $ nf ordNub l1

    -- -- , bench "1000 ordNubState" $ nf ordNubState l1000
    -- -- , bench "500  ordNubState" $ nf ordNubState l500
    , bench "100  ordNubState" $ nf ordNubState l100
    , bench "50   ordNubState" $ nf ordNubState l50
    , bench "10   ordNubState" $ nf ordNubState l10
    , bench "5    ordNubState" $ nf ordNubState l5
    , bench "1    ordNubState" $ nf ordNubState l1

    -- , bench "1000 ordNubStateDlist" $ nf ordNubStateDlist l1000
    -- , bench "500  ordNubStateDlist" $ nf ordNubStateDlist l500
    , bench "100  ordNubStateDlist" $ nf ordNubStateDlist l100
    , bench "50   ordNubStateDlist" $ nf ordNubStateDlist l50
    , bench "10   ordNubStateDlist" $ nf ordNubStateDlist l10
    , bench "5    ordNubStateDlist" $ nf ordNubStateDlist l5
    , bench "1    ordNubStateDlist" $ nf ordNubStateDlist l1
    ]
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
                         , isLikeNub ordNubStateDlist
                         ]
  where
    isLikeNub f = property (\l -> nub l == f (l :: [Int]))
