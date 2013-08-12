module Main where

import Control.Monad.State.Strict
import Data.Int (Int64)
import Data.List (sort, nub)
import Data.Set (empty, fromList, insert, member)
import qualified Data.Set as Set
import qualified Data.HashSet as H
import qualified Data.Hashable as H

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
ordNub l = go empty l
  where
    go _ []     = []
    go s (x:xs) = if x `member` s then go s xs
                                  else x : go (insert x s) xs

-- Taken From Yi
hashNub :: (H.Hashable a, Eq a) => [a] -> [a]
hashNub l = go H.empty l
  where
    go _ []     = []
    go s (x:xs) = if x `H.member` s then go s xs
                                    else x : go (H.insert x s) xs




-- Using a state monad
ordNubState :: (Ord a) => [a] -> [a]
ordNubState xs = evalState (filterM f xs) empty
  where
    f x = do set <- get
             if member x set
               then return False
               else put (insert x set) >> return True


-- Using a state monad with a dlist instead of filterM
ordNubStateDlist :: (Ord a) => [a] -> [a]
ordNubStateDlist l = evalState (f l id) empty
  where
    f []     dlist = return $ dlist []
    f (x:xs) dlist = do set <- get
                        if member x set
                          then f xs dlist
                          else put (insert x set) >> f xs (dlist . (x:))


sortNub :: Ord a => [a] -> [a]
sortNub = Set.toAscList . Set.fromList

sortNubDef :: Ord a => [a] -> [a]
sortNubDef = sort . ordNub

sortNubDef2 :: Ord a => [a] -> [a]
sortNubDef2 = sort . nub


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

    , bgroup "nub"
        -- , bench "1000" $ nf nub l1000
        -- , bench "500 " $ nf nub l500
        [ bench "100" $ nf nub l100
        , bench "50 " $ nf nub l50
        , bench "10 " $ nf nub l10
        , bench "5  " $ nf nub l5
        , bench "1  " $ nf nub l1
        ]
    -- , bgroup "localNub"
    --     -- , bench "1000" $ nf localNub l1000
    --     -- , bench "500 " $ nf localNub l500
    --     [ bench "100" $ nf localNub l100
    --     , bench "50 " $ nf localNub l50
    --     , bench "10 " $ nf localNub l10
    --     , bench "5  " $ nf localNub l5
    --     , bench "1  " $ nf localNub l1
    --     ]
    , bgroup "ordNub"
        [ bench "1000" $ nf ordNub l1000
        , bench "500 " $ nf ordNub l500
        , bench "100" $ nf ordNub l100
        , bench "50 " $ nf ordNub l50
        , bench "10 " $ nf ordNub l10
        , bench "5  " $ nf ordNub l5
        , bench "1  " $ nf ordNub l1
        ]
    , bgroup "hashNub"
        [ bench "1000" $ nf hashNub l1000
        , bench "500 " $ nf hashNub l500
        , bench "100" $ nf hashNub l100
        , bench "50 " $ nf hashNub l50
        , bench "10 " $ nf hashNub l10
        , bench "5  " $ nf hashNub l5
        , bench "1  " $ nf hashNub l1
        ]
    , bgroup "ordNubState"
       -- , bench "1000" $ nf ordNubState l1000
       -- , bench "500 " $ nf ordNubState l500
       [ bench "100" $ nf ordNubState l100
       , bench "50 " $ nf ordNubState l50
       , bench "10 " $ nf ordNubState l10
       , bench "5  " $ nf ordNubState l5
       , bench "1  " $ nf ordNubState l1
       ]
    , bgroup "ordNubStateDlist"
       -- , bench "1000" $ nf ordNubStateDlist l1000
       -- , bench "500 " $ nf ordNubStateDlist l500
       [ bench "100" $ nf ordNubStateDlist l100
       , bench "50 " $ nf ordNubStateDlist l50
       , bench "10 " $ nf ordNubStateDlist l10
       , bench "5  " $ nf ordNubStateDlist l5
       , bench "1  " $ nf ordNubStateDlist l1
       ]
    , bgroup "sorting"
        [ bgroup "sortNub"
          [ bench "1000" $ nf sortNub l1000
          , bench "500 " $ nf sortNub l500
          , bench "100" $ nf sortNub l100
          , bench "50 " $ nf sortNub l50
          , bench "10 " $ nf sortNub l10
          , bench "5  " $ nf sortNub l5
          , bench "1  " $ nf sortNub l1
          ]
        , bgroup "sort . ordNub"
          [ bench "1000" $ nf sortNubDef l1000
          , bench "500 " $ nf sortNubDef l500
          , bench "100" $ nf sortNubDef l100
          , bench "50 " $ nf sortNubDef l50
          , bench "10 " $ nf sortNubDef l10
          , bench "5  " $ nf sortNubDef l5
          , bench "1  " $ nf sortNubDef l1
          ]
        , bgroup "sort . nub"
          [ bench "1000" $ nf sortNubDef2 l1000
          , bench "500 " $ nf sortNubDef2 l500
          , bench "100" $ nf sortNubDef2 l100
          , bench "50 " $ nf sortNubDef2 l50
          , bench "10 " $ nf sortNubDef2 l10
          , bench "5  " $ nf sortNubDef2 l5
          , bench "1  " $ nf sortNubDef2 l1
          ]
        ]
    ]
  ]
  where
    l1000 = concat $ replicate 10 [1..1000::Int]
    l500  = concat $ replicate 20  [1..500::Int]
    l100  = concat $ replicate 100 [1..100::Int]
    l50   = concat $ replicate 200  [1..50::Int]
    l10   = concat $ replicate 1000 [1..10::Int]
    l5    = concat $ replicate 2000  [1..5::Int]
    l1    = concat $ replicate 10000    [1::Int]


tests :: IO ()
tests = mapM_ quickCheck [ isLikeNub localNub
                         , isLikeNub ordNub
                         , isLikeNub hashNub
                         , isLikeNub ordNubState
                         , isLikeNub ordNubStateDlist
                         , sortSame
                         ]
  where
    isLikeNub f = property (\l -> nub l == f (l :: [Int]))
    sortSame = property (\l -> sortNub l == sort (nub (l :: [Int])))
