# Interface to OpenML Data Sets

This is the class for data sets served on
[OpenML](https://www.openml.org/search?type=data&status=active). This
object can also be constructed using the sugar function
[`odt()`](https://mlr3oml.mlr-org.com/dev/reference/odt.md).

## mlr3 Integration

- A [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) can be
  obtained by calling
  [`mlr3::as_task()`](https://mlr3.mlr-org.com/reference/as_task.html).
  The target column must either be the default target (this is the
  default behaviour) or one of `$feature_names`. In case the target is
  specified to be one of `$feature_names`, the default target is added
  to the features of the task.

- A
  [mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html)
  can be obtained by calling
  [`mlr3::as_data_backend()`](https://mlr3.mlr-org.com/reference/as_data_backend.html).
  Depending on the selected file-type, the returned backend is a
  [mlr3::DataBackendDataTable](https://mlr3.mlr-org.com/reference/DataBackendDataTable.html)
  (arff) or
  [mlr3db::DataBackendDuckDB](https://mlr3db.mlr-org.com/reference/DataBackendDuckDB.html)
  (parquet). Note that a converted backend can contain columns beyond
  the target and the features (id column or ignore columns).

## Name conversion

Column names that don't comply with R's naming scheme are renamed (see
[`base::make.names()`](https://rdrr.io/r/base/make.names.html)). This
means that the names can differ from those on OpenML.

## File Format

The datasets stored on OpenML are either stored as (sparse) ARFF or
parquet. When creating a new `OMLData` object, the constructor argument
`parquet` allows to switch between arff and parquet. Note that not
necessarily all data files are available as parquet. The option
`mlr3oml.parquet` can be used to set a default. If `parquet` is `TRUE`
but not available, `"arff"` will be used as a fallback.

## ARFF Files

This package comes with an own reader for ARFF files, based on
[`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html).
For sparse ARFF files and if the
[RWeka](https://CRAN.R-project.org/package=RWeka) package is installed,
the reader automatically falls back to the implementation in
([`RWeka::read.arff()`](https://rdrr.io/pkg/RWeka/man/read.arff.html)).

## Parquet Files

For the handling of parquet files, we rely on
[duckdb](https://CRAN.R-project.org/package=duckdb) and
[DBI](https://CRAN.R-project.org/package=DBI).

## References

Vanschoren J, van Rijn JN, Bischl B, Torgo L (2014). “OpenML.” *ACM
SIGKDD Explorations Newsletter*, **15**(2), 49–60.
[doi:10.1145/2641190.2641198](https://doi.org/10.1145/2641190.2641198) .

## Super class

[`mlr3oml::OMLObject`](https://mlr3oml.mlr-org.com/dev/reference/oml_object.md)
-\> `OMLData`

## Active bindings

- `qualities`:

  ([`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Data set qualities (performance values), downloaded from the JSON API
  response and converted to a
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with columns `"name"` and `"value"`.

- `tags`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Returns all tags of the object.

- `parquet`:

  (`logical(1)`)  
  Whether to use parquet.

- `data`:

  ([`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Returns the data (without the row identifier and ignore id columns).

- `features`:

  ([`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Information about data set features (including target), downloaded
  from the JSON API response and converted to a
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with columns:

  - `"index"` ([`integer()`](https://rdrr.io/r/base/integer.html)):
    Column position.

  - `"name"` ([`character()`](https://rdrr.io/r/base/character.html)):
    Name of the feature.

  - `"data_type"` ([`factor()`](https://rdrr.io/r/base/factor.html)):
    Type of the feature: `"nominal"` or `"numeric"`.

  - `"nominal_value"` ([`list()`](https://rdrr.io/r/base/list.html)):
    Levels of the feature, or `NULL` for numeric features.

  - `"is_target"` ([`logical()`](https://rdrr.io/r/base/logical.html)):
    `TRUE` for target column, `FALSE` otherwise.

  - `"is_ignore"` ([`logical()`](https://rdrr.io/r/base/logical.html)):
    `TRUE` if this feature should be ignored. Ignored features are
    removed automatically from the data set.

  - `"is_row_identifier"`
    ([`logical()`](https://rdrr.io/r/base/logical.html)): `TRUE` if the
    column encodes a row identifier. Row identifiers are removed
    automatically from the data set.

  - `"number_of_missing_values"`
    ([`integer()`](https://rdrr.io/r/base/integer.html)): Number of
    missing values in the column.

- `target_names`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Name of the default target, as extracted from the OpenML data set
  description.

- `feature_names`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Name of the features, as extracted from the OpenML data set
  description.

- `nrow`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Number of observations, as extracted from the OpenML data set
  qualities.

- `ncol`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Number of features (including targets), as extracted from the table of
  data set features. This excludes row identifiers and ignored columns.

- `license`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Returns all license of the dataset.

- `parquet_path`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Downloads the parquet file (or loads from cache) and returns the path
  of the parquet file. Note that this also normalizes the names of the
  parquet file.

## Methods

### Public methods

- [`OMLData$new()`](#method-OMLData-new)

- [`OMLData$print()`](#method-OMLData-print)

- [`OMLData$download()`](#method-OMLData-download)

- [`OMLData$quality()`](#method-OMLData-quality)

- [`OMLData$clone()`](#method-OMLData-clone)

Inherited methods

- [`mlr3oml::OMLObject$help()`](https://mlr3oml.mlr-org.com/dev/reference/OMLObject.html#method-help)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    OMLData$new(
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
[mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) via
[`as_task()`](https://mlr3.mlr-org.com/reference/as_task.html).

#### Usage

    OMLData$print()

------------------------------------------------------------------------

### Method `download()`

Downloads the whole object for offline usage.

#### Usage

    OMLData$download()

------------------------------------------------------------------------

### Method `quality()`

Returns the value of a single OpenML data set quality.

#### Usage

    OMLData$quality(name)

#### Arguments

- `name`:

  (`character(1)`)  
  Name of the quality to extract.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OMLData$clone(deep = FALSE)

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
