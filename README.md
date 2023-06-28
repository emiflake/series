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

While there are as of yet no proper benchmarks in order to assess the performance of this library. The current implementation is backed by a `Vector`, and (ab)uses the `O(1)` slicing, and binary search in order to make operations more optimal than a naive implementation.

As an example, slicing `Series` with a list leads to `O(n)` slice. With a Vector, and binary search, we get `O(log n)` slice.

## Prior work

- [time-series](https://hackage.haskell.org/package/time-series) (github private, no active maintainer)

  The primary inspiration for this package. The underlying data structure is a list.
 
- [timeseries](https://hackage.haskell.org/package/timeseries) ([github](https://github.com/klangner/timeseries))

  Seemingly very similar to `time-series`. Also uses a list. Has a notion of "time resolution" which is not yet implemented in this library.
