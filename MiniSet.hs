-- Taken from
-- <https://github.com/haskell/containers/blob/4634081498663590925938663c0223d5e02eb8ff/Data/Set/Base.hs>,
-- see the file for licensing information.
{-# LANGUAGE BangPatterns #-}
module MiniSet (ordNub, ordNubStrictAcc) where

-- See Note: Order of constructors
data Set a    = Bin {-# UNPACK #-} !Size !a !(Set a) !(Set a)
              | Tip

type Size     = Int

-- | /O(1)/. Create a singleton set.
singleton :: a -> Set a
singleton x = Bin 1 x Tip Tip
{-# INLINE singleton #-}

-- See Note: Type of local 'go' function
insert :: Ord a => a -> Set a -> Set a
insert = go
  where
    go :: Ord a => a -> Set a -> Set a
    go !x Tip = singleton x
    go !x (Bin sz y l r) = case compare x y of
        LT -> balanceL y (go x l) r
        GT -> balanceR y l (go x r)
        EQ -> Bin sz x l r
{-# INLINABLE insert #-}

balanceL :: a -> Set a -> Set a -> Set a
balanceL x l r = case r of
  Tip -> case l of
           Tip -> Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) -> Bin 2 x l Tip
           (Bin _ lx Tip (Bin _ lrx _ _)) -> Bin 3 lrx (Bin 1 lx Tip Tip) (Bin 1 x Tip Tip)
           (Bin _ lx ll@(Bin _ _ _ _) Tip) -> Bin 3 lx ll (Bin 1 x Tip Tip)
           (Bin ls lx ll@(Bin lls _ _ _) lr@(Bin lrs lrx lrl lrr))
             | lrs < ratio*lls -> Bin (1+ls) lx ll (Bin (1+lrs) x lr Tip)
             | otherwise -> Bin (1+ls) lrx (Bin (1+lls+size lrl) lx ll lrl) (Bin (1+size lrr) x lrr Tip)

  (Bin rs _ _ _) -> case l of
           Tip -> Bin (1+rs) x Tip r

           (Bin ls lx ll lr)
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _, Bin lrs lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lx ll (Bin (1+rs+lrs) x lr r)
                     | otherwise -> Bin (1+ls+rs) lrx (Bin (1+lls+size lrl) lx ll lrl) (Bin (1+rs+size lrr) x lrr r)
                   (_, _) -> error "Failure in Data.Map.balanceL"
              | otherwise -> Bin (1+ls+rs) x l r
{-# NOINLINE balanceL #-}

balanceR :: a -> Set a -> Set a -> Set a
balanceR x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) -> Bin 2 x Tip r
           (Bin _ rx Tip rr@(Bin _ _ _ _)) -> Bin 3 rx (Bin 1 x Tip Tip) rr
           (Bin _ rx (Bin _ rlx _ _) Tip) -> Bin 3 rlx (Bin 1 x Tip Tip) (Bin 1 rx Tip Tip)
           (Bin rs rx rl@(Bin rls rlx rll rlr) rr@(Bin rrs _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rx (Bin (1+rls) x Tip rl) rr
             | otherwise -> Bin (1+rs) rlx (Bin (1+size rll) x Tip rll) (Bin (1+rrs+size rlr) rx rlr rr)

  (Bin ls _ _ _) -> case r of
           Tip -> Bin (1+ls) x l Tip

           (Bin rs rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlx rll rlr, Bin rrs _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rx (Bin (1+ls+rls) x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlx (Bin (1+ls+size rll) x l rll) (Bin (1+rrs+size rlr) rx rlr rr)
                   (_, _) -> error "Failure in Data.Map.balanceR"
              | otherwise -> Bin (1+ls+rs) x l r
{-# NOINLINE balanceR #-}

delta,ratio :: Int
delta = 3
ratio = 2

size :: Set a -> Int
size Tip = 0
size (Bin sz _ _ _) = sz
{-# INLINE size #-}

empty  :: Set a
empty = Tip
{-# INLINE empty #-}

-- | /O(log n)/. Is the element in the set?
member :: Ord a => a -> Set a -> Bool
member = go
  where
    go !_ Tip = False
    go !x (Bin _ y l r) = case compare x y of
      LT -> go x l
      GT -> go x r
      EQ -> True
{-# INLINABLE member #-}

-- We define 'ordNub' here to give GHC as many opportunities to
-- optimize as possible.

ordNub :: Ord a => [a] -> [a]
ordNub = go empty
  where
    go _ [] = []
    go s (x : xs) =
      if member x s
        then go s xs
        else x : go (insert x s) xs

ordNubStrictAcc :: Ord a => [a] -> [a]
ordNubStrictAcc = go empty
  where
    go !_ [] = []
    go !s (x : xs) =
      if member x s
        then go s xs
        else x : go (insert x s) xs
