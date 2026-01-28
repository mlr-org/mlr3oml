# Syntactic Sugar for Run Construction

Creates an
[`OMLRun`](https://mlr3oml.mlr-org.com/dev/reference/oml_run.md)
instance.

## Usage

``` r
orn(id, parquet = parquet_default(), test_server = test_server_default())
```

## Arguments

- id:

  (`integer(1)`)  
  OpenML id for the object.

- parquet:

  (`logical(1)`)  
  Whether to use parquet instead of arff. If parquet is not available,
  it will fall back to arff. Defaults to value of option
  `"mlr3oml.parquet"` or `FALSE` if not set.

- test_server:

  (`character(1)`)  
  Whether to use the OpenML test server or public server. Defaults to
  value of option `"mlr3oml.test_server"`, or `FALSE` if not set.

## Value

([`OMLRun`](https://mlr3oml.mlr-org.com/dev/reference/oml_run.md))
