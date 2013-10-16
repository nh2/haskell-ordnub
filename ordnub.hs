{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.State.Strict
import qualified Control.Monad.State.Lazy as SL
import Data.Function (on)
import Data.List (nub, nubBy)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.QuickCheck
import Test.QuickCheck.Function
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


-- Using a lazy state monad
ordNubStateLazy :: (Ord a) => [a] -> [a]
ordNubStateLazy xs = SL.evalState (filterM f xs) Set.empty
  where
    f x = do set <- SL.get
             if Set.member x set
               then return False
               else SL.put (Set.insert x set) >> return True


-- Using a state monad with a dlist instead of filterM
ordNubStateDlist :: (Ord a) => [a] -> [a]
ordNubStateDlist l = evalState (f l id) Set.empty
  where
    f []     dlist = return $ dlist []
    f (x:xs) dlist = do set <- get
                        if Set.member x set
                          then f xs dlist
                          else put (Set.insert x set) >> f xs (dlist . (x:))

-- Using a lazy state monad with a dlist instead of filterM
ordNubStateLazyDlist :: (Ord a) => [a] -> [a]
ordNubStateLazyDlist l = SL.evalState (f l id) Set.empty
  where
    f []     dlist = return $ dlist []
    f (x:xs) dlist = do set <- SL.get
                        if Set.member x set
                          then f xs dlist
                          else SL.put (Set.insert x set) >> f xs (dlist . (x:))


-- When removing duplicates, the first function assigns the input to a bucket,
-- the second function checks whether it is already in the bucket (linear search).
ordNubBy :: (Ord b) => (a -> b) -> (a -> a -> Bool) -> [a] -> [a]
ordNubBy p f l = go Map.empty l
  where
    go _ []     = []
    go m (x:xs) = let b = p x in case b `Map.lookup` m of
                    Nothing     -> x : go (Map.insert b [x] m) xs
                    Just bucket
                      | elem_by f x bucket -> go m xs
                      | otherwise          -> x : go (Map.insert b (x:bucket) m) xs

    -- From the Data.List source code.
    elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
    elem_by _  _ []     = False
    elem_by eq y (x:xs) = y `eq` x || elem_by eq y xs


main :: IO ()
main = defaultMain
  [ bgroup "simple"
    [ bench "nub [1]"       $ nf nub [1::Int]
    , bench "nub [1..10]"   $ nf nub [1..10::Int]
    , bench "nub [1..100]"  $ nf nub [1..100::Int]
    , bench "nub [1..1000]" $ nf nub [1..1000::Int]
    , bench "nub (replicate 1000 1)" $ nf nub (replicate 1000 (1::Int))

    , bench "ordNub [1]"       $ nf ordNub [1::Int]
    , bench "ordNub [1..10]"   $ nf ordNub [1..10::Int]
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

    -- , bench "1000 ordNubStateLazy" $ nf ordNubStateLazy l1000
    -- , bench "500  ordNubStateLazy" $ nf ordNubStateLazy l500
    , bench "100  ordNubStateLazy" $ nf ordNubStateLazy l100
    , bench "50   ordNubStateLazy" $ nf ordNubStateLazy l50
    , bench "10   ordNubStateLazy" $ nf ordNubStateLazy l10
    , bench "5    ordNubStateLazy" $ nf ordNubStateLazy l5
    , bench "1    ordNubStateLazy" $ nf ordNubStateLazy l1

    -- , bench "1000 ordNubStateDlist" $ nf ordNubStateDlist l1000
    -- , bench "500  ordNubStateDlist" $ nf ordNubStateDlist l500
    , bench "100  ordNubStateDlist" $ nf ordNubStateDlist l100
    , bench "50   ordNubStateDlist" $ nf ordNubStateDlist l50
    , bench "10   ordNubStateDlist" $ nf ordNubStateDlist l10
    , bench "5    ordNubStateDlist" $ nf ordNubStateDlist l5
    , bench "1    ordNubStateDlist" $ nf ordNubStateDlist l1

    -- , bench "1000 ordNubStateLazyDlist" $ nf ordNubStateLazyDlist l1000
    -- , bench "500  ordNubStateLazyDlist" $ nf ordNubStateLazyDlist l500
    , bench "100  ordNubStateLazyDlist" $ nf ordNubStateLazyDlist l100
    , bench "50   ordNubStateLazyDlist" $ nf ordNubStateLazyDlist l50
    , bench "10   ordNubStateLazyDlist" $ nf ordNubStateLazyDlist l10
    , bench "5    ordNubStateLazyDlist" $ nf ordNubStateLazyDlist l5
    , bench "1    ordNubStateLazyDlist" $ nf ordNubStateLazyDlist l1


    -- `by` functions

    -- , bench "1000 nubBy" $ nf (nubBy (\a b -> a `quot` 2 == b `quot` 2) (==)) l1000
    -- , bench "500  nubBy" $ nf (nubBy (\a b -> a `quot` 2 == b `quot` 2) (==)) l500
    , bench "100  nubBy" $ nf (nubBy (\a b -> a `quot` 2 == b `quot` 2)) l100
    , bench "50   nubBy" $ nf (nubBy (\a b -> a `quot` 2 == b `quot` 2)) l50
    , bench "10   nubBy" $ nf (nubBy (\a b -> a `quot` 2 == b `quot` 2)) l10
    , bench "5    nubBy" $ nf (nubBy (\a b -> a `quot` 2 == b `quot` 2)) l5
    , bench "1    nubBy" $ nf (nubBy (\a b -> a `quot` 2 == b `quot` 2)) l1

    -- , bench "1000 ordNubBy" $ nf (ordNubBy (`quot` 2) (==)) l1000
    -- , bench "500  ordNubBy" $ nf (ordNubBy (`quot` 2) (==)) l500
    , bench "100  ordNubBy" $ nf (ordNubBy (`quot` 2) (==)) l100
    , bench "50   ordNubBy" $ nf (ordNubBy (`quot` 2) (==)) l50
    , bench "10   ordNubBy" $ nf (ordNubBy (`quot` 2) (==)) l10
    , bench "5    ordNubBy" $ nf (ordNubBy (`quot` 2) (==)) l5
    , bench "1    ordNubBy" $ nf (ordNubBy (`quot` 2) (==)) l1
    ]

  -- Other benchmarks, and what people contributed
  , bgroup "other"
    [ bench "nub yitz 1"    $ nf nub    (2 : replicate 100000 1 ++ [3] :: [Int])
    , bench "ordNub yitz 1" $ nf ordNub (2 : replicate 100000 1 ++ [3] :: [Int])

    , bench "nub yitz 2"    $ nf nub    ([3,2,1] ++ take 100000 (cycle [3,2,1]) ++ [4] :: [Int])
    , bench "ordNub yitz 2" $ nf ordNub ([3,2,1] ++ take 100000 (cycle [3,2,1]) ++ [4] :: [Int])
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
tests = mapM_ (quickCheckWith stdArgs{ maxSuccess = 1000, maxSize = 200 })
  [ isLikeNub localNub
  , isLikeNub ordNub
  , isLikeNub ordNubState
  , isLikeNub ordNubStateDlist

  -- ordNubBy tests
  , property $ \(l :: [(Int, Int)]) -> ordNubBy fst ((>) `on` snd) l
                                       == nubBy (\(a,b) (x,y) -> a == x && b > y) l

  , property $ \(l :: [(Int, Int)], Fun _ f :: Fun Int (Fun Int Bool)) ->
      let fun x y = f x `apply` y
       in ordNubBy fst (\(_, b) (_, y) ->           b `fun` y) l ==
          nubBy        (\(a,b) (x,y)   -> a == x && b `fun` y) l
  ]
  where
    isLikeNub f = property (\l -> nub l == f (l :: [Int]))
