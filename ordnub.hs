{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.State.Strict
import Data.Function (on)
import Data.List (nub, nubBy)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.QuickCheck
import Test.QuickCheck.Function
import Criterion.Main

import qualified Data.Set as S
import Data.Set (Set)


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


-- Richard Bird's derivation from "Functional Pearls of Algorithm Design"
birdNub :: Ord a => [a] -> [a]
birdNub = hub' S.empty S.empty . preprocess

-- Why not nub = S.toList . S.fromList ?

preprocess :: Ord a => [a] -> [(a, Set a)]
preprocess xs = zip xs (tail (scanr S.insert S.empty xs))

hub' ps ws [] = []
hub' ps ws ((x,xs):xss) =
  if S.member x ps then hub' ps ws xss else
    case (S.member x xs, S.member x ws) of
      (False, False) -> eus ++ [x] ++ hub' qs S.empty xss
      (False, True)  -> eus ++ [x] ++ hub' qs vs xss
      (True, False)  -> hub' ps (S.insert x us) xss
      (True, True)   -> hub' ps ws xss
    where (us, vs) = S.split x ws
          eus      = S.elems us
          qs       = foldr S.insert ps eus

setNub :: Ord a => [a] -> [a]
setNub = S.toList . S.fromList

main :: IO ()
main = defaultMain
  [ bgroup "simple"
    [ bench "nub [1..100]"  $ nf nub [1..100::Int]
    , bench "nub [1..1000]" $ nf nub [1..1000::Int]
    , bench "nub (replicate 1000 1)" $ nf nub (replicate 1000 (1::Int))

    , bench "ordNub [1..100]"  $ nf ordNub [1..100::Int]
    , bench "ordNub [1..1000]" $ nf ordNub [1..1000::Int]
    , bench "ordNub (replicate 1000 1)" $ nf ordNub (replicate 1000 (1::Int))

    , bench "birdNub [1..100]"  $ nf birdNub [1..100::Int]
    , bench "birdNub [1..1000]" $ nf birdNub [1..1000::Int]
    , bench "birdNub (replicate 1000 1)" $ nf birdNub (replicate 1000 (1::Int))

    , bench "setNub [1..100]"  $ nf setNub [1..100::Int]
    , bench "setNub [1..1000]" $ nf setNub [1..1000::Int]
    , bench "setNub (replicate 1000 1)" $ nf setNub (replicate 1000 (1::Int))
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

    -- , bench "1000 birdNub" $ nf birdNub l1000
    -- , bench "500  birdNub" $ nf birdNub l500
    , bench "100  birdNub" $ nf birdNub l100
    , bench "50   birdNub" $ nf birdNub l50
    , bench "10   birdNub" $ nf birdNub l10
    , bench "5    birdNub" $ nf birdNub l5
    , bench "1    birdNub" $ nf birdNub l1

    -- , bench "1000 setNub" $ nf setNub l1000
    -- , bench "500  setNub" $ nf setNub l500
    , bench "100  setNub" $ nf setNub l100
    , bench "50   setNub" $ nf setNub l50
    , bench "10   setNub" $ nf setNub l10
    , bench "5    setNub" $ nf setNub l5
    , bench "1    setNub" $ nf setNub l1

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
  , isLikeNub birdNub
  , isLikeNub setNub

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
