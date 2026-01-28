# Syntactic Sugar for Collection Construction

Creates an
[`OMLCollection`](https://mlr3oml.mlr-org.com/reference/oml_collection.md)
instance.

## Usage

``` r
ocl(id, test_server = test_server_default())
```

## Arguments

- id:

  (`integer(1)`)  
  OpenML id for the object.

- test_server:

  (`character(1)`)  
  Whether to use the OpenML test server or public server. Defaults to
  value of option `"mlr3oml.test_server"`, or `FALSE` if not set.

## Value

([`OMLCollection`](https://mlr3oml.mlr-org.com/reference/oml_collection.md))
