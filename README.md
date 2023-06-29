# series

Useful Haskell functions for interacting with timed data.

A `Series` is a _sequence_ of `DataPoint`s, each `DataPoint` contains the _time_ the data point occurs at, and the value. Reasoning about this structure, we can create various domain-specific operations to perform on these.

## Features

- [x] Time slicing
- [x] Mapping
- [x] Resampling
- [x] Merging data
- [ ] Importing/exporting from CSV
- [ ] Diagram generation

## Performance

The current implementation is backed by a `Vector`, and (ab)uses the `O(1)` slicing, and binary search in order to make operations more optimal than a naive implementation.

As an example, slicing `Series` with a list leads to `O(n)` slice. With a Vector, and binary search, we get `O(log n)` slice.

Here is a comparison of naive and vector-backed slicing:

```
benchmarking slice/series
time                 271.5 ns   (270.1 ns .. 273.2 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 272.5 ns   (271.5 ns .. 273.6 ns)
std dev              3.600 ns   (2.779 ns .. 4.583 ns)
variance introduced by outliers: 13% (moderately inflated)

benchmarking slice/naive
time                 1.119 ms   (1.113 ms .. 1.126 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.106 ms   (1.102 ms .. 1.114 ms)
std dev              21.08 μs   (16.63 μs .. 30.04 μs)
```

More benchmarks may follow.

## Prior work

- [time-series](https://hackage.haskell.org/package/time-series) (github private, no active maintainer)

  The primary inspiration for this package. The underlying data structure is a list.
 
- [timeseries](https://hackage.haskell.org/package/timeseries) ([github](https://github.com/klangner/timeseries))

  Seemingly very similar to `time-series`. Also uses a list. Has a notion of "time resolution" which is not yet implemented in this library.
