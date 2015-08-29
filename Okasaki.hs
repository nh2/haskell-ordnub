{-# LANGUAGE BangPatterns #-}
module Okasaki (ordNub, ordNubStrictAcc) where

data Color = R | B deriving Show
data RB a = E | T Color (RB a) a (RB a) deriving Show

{- Insertion and membership test as by Okasaki -}
insertRB :: (a -> a -> Ordering) -> a -> RB a -> RB a
insertRB cmp x s =
    T B a z b
    where
    T _ a z b = ins s
    ins E = T R E x E
    ins s@(T B a y b) = case cmp x y of
        LT -> balance (ins a) y b
        GT -> balance a y (ins b)
        EQ -> s
    ins s@(T R a y b) = case cmp x y of
        LT -> T R (ins a) y b
        GT -> T R a y (ins b)
        EQ -> s

memberRB :: (a -> a -> Ordering) -> a -> RB a -> Bool
memberRB cmp x E = False
memberRB cmp x (T _ a y b) = case cmp x y of
    LT -> memberRB cmp x a
    GT -> memberRB cmp x b
    EQ -> True

{- balance: first equation is new,
   to make it work with a weaker invariant -}
balance :: RB a -> a -> RB a -> RB a
balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance a x b = T B a x b


-- We define 'ordNub' here to give GHC as many opportunities to
-- optimize as possible.

ordNub :: Ord a => [a] -> [a]
ordNub = go E
  where
    go _ [] = []
    go s (x : xs) =
      if memberRB compare x s
        then go s xs
        else x : go (insertRB compare x s) xs

ordNubStrictAcc :: Ord a => [a] -> [a]
ordNubStrictAcc = go E
  where
    go !_ [] = []
    go !s (x : xs) =
      if memberRB compare x s
        then go s xs
        else x : go (insertRB compare x s) xs
