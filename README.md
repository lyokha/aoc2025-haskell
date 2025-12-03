My solution for the [Advent of Code 2025](https://adventofcode.com/2025) puzzles
crafted with Haskell.

Benchmarks were taken on my *Intel(R) Core(TM) i5-8250U CPU @ 1.60GHz* computer
with the original inputs appointed from the Advent of Code.

```ShellSession
$ hyperfine -w 2 -r 5 './main 1'
Benchmark 1: ./main 1
  Time (mean ± σ):      16.9 ms ±   1.6 ms    [User: 13.5 ms, System: 3.4 ms]
  Range (min … max):    14.1 ms …  18.0 ms    5 runs

$ hyperfine -w 2 -r 5 './main 2'
Benchmark 1: ./main 2
  Time (mean ± σ):       9.6 ms ±   1.7 ms    [User: 5.7 ms, System: 3.8 ms]
  Range (min … max):     7.0 ms …  11.4 ms    5 runs

$ hyperfine -w 2 -r 5 './main 3'
Benchmark 1: ./main 3
  Time (mean ± σ):      13.7 ms ±   0.7 ms    [User: 10.3 ms, System: 3.4 ms]
  Range (min … max):    12.8 ms …  14.5 ms    5 runs
```

