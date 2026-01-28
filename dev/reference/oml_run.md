# Interface to OpenML Runs

This is the class for OpenML
[Runs](https://www.openml.org/search?type=run&sort=date), which are
conceptually similar to
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)s.
This object can also be constructed using the sugar function
`oml_run()`.

## OpenML Integration

- A [OMLTask](https://mlr3oml.mlr-org.com/dev/reference/oml_task.md) is
  returned by accessing the active field `$task`.

- A [OMLData](https://mlr3oml.mlr-org.com/dev/reference/oml_data.md) is
  returned by accessing the active field `$data` (short for
  `$task$data`)

- A [OMLFlow](https://mlr3oml.mlr-org.com/dev/reference/oml_flow.md) is
  returned by accessing the active field `$flow`.

- The raw predictions are returned by accessing the active field
  `$prediction`.

## mlr3 Integration

- A
  [mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
  is returned when calling
  [`mlr3::as_resample_result()`](https://mlr3.mlr-org.com/reference/as_resample_result.html).

- A [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) is
  returned when calling
  [`mlr3::as_task()`](https://mlr3.mlr-org.com/reference/as_task.html).

- A
  [mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html)
  is returned when calling
  [`mlr3::as_data_backend()`](https://mlr3.mlr-org.com/reference/as_data_backend.html).

- A instantiated
  [mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)
  is returned when calling
  [`mlr3::as_resampling()`](https://mlr3.mlr-org.com/reference/as_resampling.html).

## References

Vanschoren J, van Rijn JN, Bischl B, Torgo L (2014). “OpenML.” *ACM
SIGKDD Explorations Newsletter*, **15**(2), 49–60.
[doi:10.1145/2641190.2641198](https://doi.org/10.1145/2641190.2641198) .

## Super class

[`mlr3oml::OMLObject`](https://mlr3oml.mlr-org.com/dev/reference/oml_object.md)
-\> `OMLRun`

## Active bindings

- `flow_id`:

  (`integer(1)`)  
  The id of the flow.

- `flow`:

  ([OMLFlow](https://mlr3oml.mlr-org.com/dev/reference/oml_flow.md))  
  The OpenML Flow.

- `tags`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Returns all tags of the object.

- `parquet`:

  (`logical(1)`)  
  Whether to use parquet.

- `task_id`:

  (`character(1)`)  
  The id of the task solved by this run.

- `task`:

  ([OMLTask](https://mlr3oml.mlr-org.com/dev/reference/oml_task.md))  
  The task solved by this run.

- `data_id`:

  (`integer(1)`)  
  The id of the dataset.

- `data`:

  ([OMLData](https://mlr3oml.mlr-org.com/dev/reference/oml_data.md))  
  The data used in this run.

- `task_type`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  The task type.

- `parameter_setting`:

  [`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  The parameter setting for this run.

- `prediction`:

  ([`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  The raw predictions of the run as returned by OpenML, not in standard
  mlr3 format. Formatted predictions are accessible after converting to
  a
  [mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
  via
  [`as_resample_result()`](https://mlr3.mlr-org.com/reference/as_resample_result.html).

- `evaluation`:

  ([`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  The evaluations calculated by the OpenML server.

## Methods

### Public methods

- [`OMLRun$new()`](#method-OMLRun-new)

- [`OMLRun$print()`](#method-OMLRun-print)

- [`OMLRun$download()`](#method-OMLRun-download)

- [`OMLRun$clone()`](#method-OMLRun-clone)

Inherited methods

- [`mlr3oml::OMLObject$help()`](https://mlr3oml.mlr-org.com/dev/reference/OMLObject.html#method-help)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    OMLRun$new(
      id,
      parquet = parquet_default(),
      test_server = test_server_default()
    )

#### Arguments

- `id`:

  (`integer(1)`)  
  OpenML id for the object.

- `parquet`:

  (`logical(1)`)  
  Whether to use parquet instead of arff. If parquet is not available,
  it will fall back to arff. Defaults to value of option
  `"mlr3oml.parquet"` or `FALSE` if not set.

- `test_server`:

  (`character(1)`)  
  Whether to use the OpenML test server or public server. Defaults to
  value of option `"mlr3oml.test_server"`, or `FALSE` if not set.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints the object.

#### Usage

    OMLRun$print()

------------------------------------------------------------------------

### Method `download()`

Downloads the whole object for offline usage.

#### Usage

    OMLRun$download()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OMLRun$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# For technical reasons, examples cannot be included in this R package.
# Instead, these are some relevant resources:
#
# Large-Scale Benchmarking chapter in the mlr3book:
# https://mlr3book.mlr-org.com/chapters/chapter11/large-scale_benchmarking.html
#
# Package Article:
# https://mlr3oml.mlr-org.com/articles/tutorial.html
```
