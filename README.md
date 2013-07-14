ordnub
======

Data.List.nub is O(n²). This one is O(n log n) by requiring an Ord instance.

* Also contains a benchmark (`report.html`) that shows that `ordNub` apparently is faster than `nub` in *all* cases.

* `PACKAGES_USING_NUB.txt` contains all packages which use `nub` (made with a quick grep).
It's not the most accurate since some packages define their own `nub`, but that's a minority.
