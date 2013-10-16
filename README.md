ordnub
======

Data.List.nub is O(n²). This one is O(n log n) by requiring an Ord instance.

* Also contains a benchmark (`report.html`) that shows that `ordNub` apparently is faster than `nub` in *all* cases.

* `PACKAGES_USING_NUB.txt` contains all packages which use `nub` (made with a quick grep).
It's not the most accurate since some packages define their own `nub`, but that's a minority.

**This thing here is not a library.** It is a benchmark suite. [View results here](http://htmlpreview.github.io/?https://github.com/nh2/haskell-ordnub/blob/master/report.html).

Don't use nub
-------------

If you are looking for a fast `ordNub` function to use in your code, you can use:

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

`\\`, `union`, `intersect`
