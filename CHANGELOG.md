# Revision history for `series`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## 0.1.1.0 -- 2023-08-09

### Added

- `Data.Series.Continuous`:
  - Expose module
- `Data.Series`:
  - Expose `findLastTimeBefore`
  
### Changed

- `Data.Series`:
  - Fix `latest` internal function which would have caused a bug previously.

## 0.1.0.0 -- 2023-06-29

### Added

- `Data.Series.Internal`:
  - `Series`: a discrete time series consisting in a collection of `DataPoint`s.
  - `DataPoint`: a data point in a `Series`.
  - `isEmpty`: determine whether a `Series` is empty.
  - `emptySeries`: get a empty `Series`.
- `Data.Series`:
  - `series`: create a `Series` from an association list.
  - `lookup`: get the value at a given time in the `Series`.
  - `(!?)`: infix version of `lookup`.
  - `slice`: slice a series from one time to another.
  - `size`: get the number of `DataPoint`s in a `Series`.
  - `singleton`: create a `Series` with only the given `DataPoint`.
  - `merge`: merge two `Series`, preserving order and duplicates.
  - `resampleSAH`: make a `Series` containing the given times, with the last value before each time in the given `Series`.
  - `bounds`: get the lower and upper bounds of a `Series`.
  - `pointwiseZipWith`: 
  - `nub`: remove duplicate times in a `Series`, always keeps the first `DataPoint` at a given time.
  - `nubWith`: remove duplicate times in a `Series`, criteria for which `DataPoint` at a given time is kept is passed as a higher order function.
  - `values`: extract the values from a `Series`, dropping the times.
  - `times`: extract the times from a `Series`, dropping the values.
