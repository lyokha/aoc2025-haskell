My solution for the [Advent of Code 2025](https://adventofcode.com/2025) puzzles
crafted with Haskell.

Benchmarks were taken on my *Intel(R) Core(TM) i5-8250U CPU @ 1.60GHz* computer
with the original inputs appointed from the Advent of Code.

```ShellSession
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.12.2
$ ghc -O --make main.hs aoc*.hs
```

```ShellSession
$ hyperfine -w 2 -r 5 './main 1'
Benchmark 1: ./main 1
  Time (mean ± σ):      13.8 ms ±   1.2 ms    [User: 9.7 ms, System: 4.1 ms]
  Range (min … max):    12.4 ms …  15.1 ms    5 runs

$ hyperfine -w 2 -r 5 './main 2'
Benchmark 1: ./main 2
  Time (mean ± σ):       7.4 ms ±   0.7 ms    [User: 3.8 ms, System: 3.5 ms]
  Range (min … max):     6.5 ms …   8.0 ms    5 runs

$ hyperfine -w 2 -r 5 './main 3'
Benchmark 1: ./main 3
  Time (mean ± σ):       9.0 ms ±   2.1 ms    [User: 7.4 ms, System: 1.9 ms]
  Range (min … max):     5.8 ms …  11.6 ms    5 runs

$ hyperfine -w 2 -r 5 './main 4'
Benchmark 1: ./main 4
  Time (mean ± σ):     262.3 ms ±   6.2 ms    [User: 249.1 ms, System: 11.1 ms]
  Range (min … max):   251.3 ms … 266.9 ms    5 runs

$ hyperfine -w 2 -r 5 './main 5'
Benchmark 1: ./main 5
  Time (mean ± σ):      12.0 ms ±   2.2 ms    [User: 8.3 ms, System: 3.6 ms]
  Range (min … max):     9.8 ms …  15.6 ms    5 runs

$ hyperfine -w 2 -r 5 './main 6'
Benchmark 1: ./main 6
  Time (mean ± σ):      17.5 ms ±   1.7 ms    [User: 14.5 ms, System: 2.9 ms]
  Range (min … max):    15.9 ms …  19.8 ms    5 runs

$ hyperfine -w 2 -r 5 './main 7'
Benchmark 1: ./main 7
  Time (mean ± σ):      16.4 ms ±   0.8 ms    [User: 13.7 ms, System: 2.7 ms]
  Range (min … max):    15.2 ms …  17.3 ms    5 runs
```

