# Read ARFF files

Parses a file located at `path` and returns a
[data.table()](https://rdrr.io/pkg/data.table/man/data.table.html).

Limitations:

- Only works for dense files, no support for sparse data. Use
  [RWeka](https://CRAN.R-project.org/package=RWeka) instead.

- Dates (even if there is no time component) are read in as
  [POSIXct](https://rdrr.io/r/base/DateTimeClasses.html).

- The `date-format` from the ARFF specification is currently ignored.
  Instead, we rely on the auto-detection of
  [data.table](https://CRAN.R-project.org/package=data.table)'s
  [fread()](https://rdrr.io/pkg/data.table/man/fread.html)..

## Usage

``` r
read_arff(path)
```

## Arguments

- path:

  (`character(1)`)  
  Path or URI of the ARFF file, passed to
  [`file()`](https://rdrr.io/r/base/connections.html).

## Value

([data.table()](https://rdrr.io/pkg/data.table/man/data.table.html)).
