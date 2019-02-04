ordnub
======

`Data.List.nub` is O(n²). This one is O(n log n) by requiring an `Ord` instance.

* Also contains a benchmark (`report.html`) that shows that `ordNub` apparently is faster than `nub` in *all* cases.

* `PACKAGES_USING_NUB.txt` contains all packages which use `nub` (made with a quick grep).
It's not the most accurate since some packages define their own `nub`, but that's a minority.

**This thing here is not a library.** It is a benchmark suite. [View results here](https://rawgit.com/nh2/haskell-ordnub/master/report.html).

Don't use nub
-------------

If you are looking for a fast `ordNub` function to use in your code, use:

* [`nubOrd`](https://hackage.haskell.org/package/containers/docs/Data-Containers-ListUtils.html) from `containers`

([stable link](https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Containers-ListUtils.html)) which after many years was finally added (also in response to this repo).

If you don't have a new enough `containers` or don't want to depend on it, use:

```haskell
import qualified Data.Set as Set

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs
```

### Other Data.List functions you NEVER want to use

`\\`, `union`, `intersect` - they too are *O(n²)*.

Also be aware that they don't work like sets. For example:

```
> [1,1,2] \\ [1]
[1,2]

> union [1,2,3,1] [1,4]
[1,2,3,1,4]
```

The current *O(n log n)* recommendation for `\\` is:

```haskell
import qualified Data.Map.Strict as Map

listDifference :: (Ord a) => [a] -> [a] -> [a]
listDifference a b = go initHist a
  where
    initHist = Map.fromListWith (+) [ (x, 1 :: Int) | x <- b ]

    go _    []     = []
    go hist (x:xs) = case Map.lookup x hist of
      Just n | n > 0 ->     go (Map.insert x (n-1) hist) xs
      _              -> x : go hist                      xs
```


The current *O(n log n)* recommendation for `union` is:

```haskell
import qualified Data.Set as Set

listUnion :: (Ord a) => [a] -> [a] -> [a]
listUnion a b = a ++ ordNub (filter (`Set.notMember` aSet) b)
  where
    aSet = Set.fromList a
```


The current *O(n log n)* recommendation for `intersect` is:

```haskell
import qualified Data.Set as Set

listIntersect :: (Ord a) => [a] -> [a] -> [a]
listIntersect a b = filter (`Set.member` bSet) a
  where
    bSet = Set.fromList b
```
