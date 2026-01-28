# Write ARFF files

Writes a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) to an
ARFF file.

Limitations:

- Logicals are written as categorical features.

- [POSIXct](https://rdrr.io/r/base/DateTimeClasses.html) columns are
  converted to UTC.

## Usage

``` r
write_arff(data, path, relation = deparse(substitute(data)))
```

## Arguments

- data:

  ([`data.frame()`](https://rdrr.io/r/base/data.frame.html))  
  Data to write.

- path:

  (`character(1)`)  
  Path or URI of the ARFF file, passed to
  [`file()`](https://rdrr.io/r/base/connections.html).

- relation:

  (`character(1)`)  
  Relation (name) of the data set.
