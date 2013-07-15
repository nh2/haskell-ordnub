module Main where

import Control.Monad.State.Strict
import Data.Int (Int64)
import Data.List (nub)
import Data.Set (empty, fromList, insert, member)

import Test.QuickCheck
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
ordNub l = go empty l
  where
    go _ []     = []
    go s (x:xs) = if x `member` s then go s xs
                                  else x : go (insert x s) xs


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
                         , isLikeNub birdNub
                         , isLikeNub setNub
                         ]
  where
    isLikeNub f = property (\l -> nub l == f (l :: [Int]))
