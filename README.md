My solution for the [Advent of Code 2025](https://adventofcode.com/2025) puzzles
crafted with Haskell.

| Day | Part 1                    | Part 2                    |
|-----|---------------------------|---------------------------|
| 1   | <ul><li>- [x] </li></ul>    | <ul><li>- [x] </li></ul>    |
| 2   | <ul><li>- [x] </li></ul>    | <ul><li>- [x] </li></ul>    |
| 3   | <ul><li>- [x] </li></ul>    | <ul><li>- [x] </li></ul>    |
| 4   | <ul><li>- [x] </li></ul>    | <ul><li>- [x] </li></ul>    |
| 5   | <ul><li>- [x] </li></ul>    | <ul><li>- [x] </li></ul>    |
| 6   | <ul><li>- [x] </li></ul>    | <ul><li>- [x] </li></ul>    |
| 7   | <ul><li>- [x] </li></ul>    | <ul><li>- [x] </li></ul>    |
| 8   | <ul><li>- [x] </li></ul>    | <ul><li>- [x] </li></ul>    |
| 9   | <ul><li>- [x] </li></ul>    | <ul><li>- [x] </li></ul>    |
| 10  | <ul><li>- [x] </li></ul>    | <ul><li>- [x] </li></ul>    |
| 11  | <ul><li>- [x] </li></ul>    | <ul><li>- [x] </li></ul>    |
| 12  | <ul><li>- [ ] </li></ul>    | <ul><li>- [ ] </li></ul>    |

Build with

```ShellSession
$ cabal build
```

Note that Haskell library *z3 408.2* is compatible with *z3 4.8*, compatibility
with newer versions is not guaranteed. To turn off building Z3, run

```ShellSession
$ cabal build -f-z3
```

or create file *cabal.project* with lines

```Cabal Config
packages: aoc2025-haskell.cabal
flags: -z3
```

Benchmarks were taken on my *Intel(R) Core(TM) i5-8250U CPU @ 1.60GHz* computer
with the original inputs appointed from the Advent of Code.

```ShellSession
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.12.2
$ cabal build
```

```ShellSession
$ hyperfine -n aoc2025 -w 2 -r 5 "$(cabal list-bin aoc2025) 1"
Benchmark 1: aoc2025
  Time (mean ± σ):      15.2 ms ±   1.2 ms    [User: 9.1 ms, System: 5.8 ms]
  Range (min … max):    13.6 ms …  16.8 ms    5 runs

$ hyperfine -n aoc2025 -w 2 -r 5 "$(cabal list-bin aoc2025) 2"
Benchmark 1: aoc2025
  Time (mean ± σ):       7.4 ms ±   0.5 ms    [User: 3.6 ms, System: 3.7 ms]
  Range (min … max):     6.8 ms …   8.0 ms    5 runs

$ hyperfine -n aoc2025 -w 2 -r 5 "$(cabal list-bin aoc2025) 3"
Benchmark 1: aoc2025
  Time (mean ± σ):       9.3 ms ±   0.4 ms    [User: 4.5 ms, System: 4.7 ms]
  Range (min … max):     8.8 ms …   9.6 ms    5 runs

$ hyperfine -n aoc2025 -w 2 -r 5 "$(cabal list-bin aoc2025) 4"
Benchmark 1: aoc2025
  Time (mean ± σ):     216.0 ms ±   8.5 ms    [User: 204.0 ms, System: 9.9 ms]
  Range (min … max):   207.1 ms … 227.0 ms    5 runs

$ hyperfine -n aoc2025 -w 2 -r 5 "$(cabal list-bin aoc2025) 5"
Benchmark 1: aoc2025
  Time (mean ± σ):      10.4 ms ±   0.6 ms    [User: 6.7 ms, System: 3.6 ms]
  Range (min … max):     9.8 ms …  11.5 ms    5 runs

$ hyperfine -n aoc2025 -w 2 -r 5 "$(cabal list-bin aoc2025) 6"
Benchmark 1: aoc2025
  Time (mean ± σ):      19.0 ms ±   1.7 ms    [User: 13.4 ms, System: 5.4 ms]
  Range (min … max):    17.3 ms …  21.8 ms    5 runs

$ hyperfine -n aoc2025 -w 2 -r 5 "$(cabal list-bin aoc2025) 7"
Benchmark 1: aoc2025
  Time (mean ± σ):      17.4 ms ±   1.0 ms    [User: 12.6 ms, System: 4.5 ms]
  Range (min … max):    16.0 ms …  18.3 ms    5 runs

$ hyperfine -n aoc2025 -w 2 -r 5 "$(cabal list-bin aoc2025) 8"
Benchmark 1: aoc2025
  Time (mean ± σ):     315.1 ms ±   5.9 ms    [User: 278.1 ms, System: 32.6 ms]
  Range (min … max):   305.4 ms … 320.4 ms    5 runs

$ hyperfine -n aoc2025 -w 2 -r 5 "$(cabal list-bin aoc2025) 9"
Benchmark 1: aoc2025
  Time (mean ± σ):     142.2 ms ±   8.4 ms    [User: 138.2 ms, System: 2.9 ms]
  Range (min … max):   135.5 ms … 153.9 ms    5 runs

$ hyperfine -n aoc2025 -w 2 -r 5 "$(cabal list-bin aoc2025) 11"
Benchmark 1: aoc2025
  Time (mean ± σ):      10.0 ms ±   0.6 ms    [User: 5.0 ms, System: 4.9 ms]
  Range (min … max):     9.2 ms …  10.8 ms    5 runs
```

