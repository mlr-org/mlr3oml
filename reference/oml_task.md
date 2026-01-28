# Interface to OpenML Tasks

This is the class for tasks served on
[OpenML](https://www.openml.org/search?type=task&sort=runs). It consists
of a dataset and other meta-information such as the target variable for
supervised problems. This object can also be constructed using the sugar
function [`otsk()`](https://mlr3oml.mlr-org.com/reference/otsk.md).

## mlr3 Integration

- Obtain a [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) by
  calling
  [`as_task()`](https://mlr3.mlr-org.com/reference/as_task.html).

- Obtain a
  [mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)
  by calling
  [`as_resampling()`](https://mlr3.mlr-org.com/reference/as_resampling.html).

## References

Vanschoren J, van Rijn JN, Bischl B, Torgo L (2014). “OpenML.” *ACM
SIGKDD Explorations Newsletter*, **15**(2), 49–60.
[doi:10.1145/2641190.2641198](https://doi.org/10.1145/2641190.2641198) .

## Super class

[`mlr3oml::OMLObject`](https://mlr3oml.mlr-org.com/reference/oml_object.md)
-\> `OMLTask`

## Active bindings

- `estimation_procedure`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  The estimation procedure, returns `NULL` if none is available.

- `task_splits`:

  ([`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  A data.table containing the splits as provided by OpenML.

- `tags`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Returns all tags of the object.

- `parquet`:

  (`logical(1)`)  
  Whether to use parquet.

- `name`:

  (`character(1)`)  
  Name of the task, extracted from the task description.

- `task_type`:

  (`character(1)`)  
  The OpenML task type.

- `data_id`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Data id, extracted from the task description.

- `data`:

  ([OMLData](https://mlr3oml.mlr-org.com/reference/oml_data.md))  
  Access to the underlying OpenML data set via a
  [OMLData](https://mlr3oml.mlr-org.com/reference/oml_data.md) object.

- `nrow`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Number of rows, extracted from the
  [OMLData](https://mlr3oml.mlr-org.com/reference/oml_data.md) object.

- `ncol`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Number of columns, as extracted from the
  [OMLData](https://mlr3oml.mlr-org.com/reference/oml_data.md) object.

- `target_names`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Name of the targets, as extracted from the OpenML task description.

- `feature_names`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Name of the features (without targets of this OMLTask).

- `data_name`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Name of the dataset (inferred from the task name).

## Methods

### Public methods

- [`OMLTask$new()`](#method-OMLTask-new)

- [`OMLTask$print()`](#method-OMLTask-print)

- [`OMLTask$download()`](#method-OMLTask-download)

- [`OMLTask$clone()`](#method-OMLTask-clone)

Inherited methods

- [`mlr3oml::OMLObject$help()`](https://mlr3oml.mlr-org.com/reference/OMLObject.html#method-help)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    OMLTask$new(
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

Prints the object. For a more detailed printer, convert to a
[mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) via `$task`.

#### Usage

    OMLTask$print()

------------------------------------------------------------------------

### Method `download()`

Downloads the whole object for offline usage.

#### Usage

    OMLTask$download()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OMLTask$clone(deep = FALSE)

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
