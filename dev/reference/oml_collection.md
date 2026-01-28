# OpenML Collection

This is the class for collections (previously known as studies) served
on <https://www.openml.org>. A collection can either be a [task
collection](https://www.openml.org/search?type=study&study_type=task) or
[run
collection](https://www.openml.org/search?type=study&study_type=run).
This object can also be constructed using the sugar function
[`ocl()`](https://mlr3oml.mlr-org.com/dev/reference/ocl.md).

**Run Collection**

A run collection contains runs, flows, datasets and tasks. The primary
object are the runs (`main_entity_type` is `"run"`). The the flows,
datasets and tasks are those used in the runs.

**Task Collection** A task collection (`main_entity_type = "task"`)
contains tasks and datasets. The primary object are the tasks
(`main_entity_type` is `"task"`). The datasets are those used in the
tasks.

*Note*: All Benchmark Suites on OpenML are also collections.

## Caching

Because collections on OpenML can be modified (ids can be added), it is
not possible to cache this object.

## mlr3 Intergration

- Obtain a list of
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)s using
  [mlr3::as_tasks](https://mlr3.mlr-org.com/reference/as_task.html).

- Obtain a list of
  [mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)s
  using
  [mlr3::as_resamplings](https://mlr3.mlr-org.com/reference/as_resampling.html).

- Obtain a list of
  [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)s
  using
  [mlr3::as_learners](https://mlr3.mlr-org.com/reference/as_learner.html)
  (if main_entity_type is "run").

- Obtain a
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)
  using
  [mlr3::as_benchmark_result](https://mlr3.mlr-org.com/reference/as_benchmark_result.html)
  (if main_entity_type is "run").

## References

Vanschoren J, van Rijn JN, Bischl B, Torgo L (2014). “OpenML.” *ACM
SIGKDD Explorations Newsletter*, **15**(2), 49–60.
[doi:10.1145/2641190.2641198](https://doi.org/10.1145/2641190.2641198) .

## Super class

[`mlr3oml::OMLObject`](https://mlr3oml.mlr-org.com/dev/reference/oml_object.md)
-\> `OMLCollection`

## Active bindings

- `desc`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Colllection description (meta information), downloaded and converted
  from the JSON API response.

- `parquet`:

  (`logical(1)`)  
  Whether to use parquet.

- `main_entity_type`:

  (`character(n)`)  
  The main entity type, either `"run"` or `"task"`.

- `flow_ids`:

  (`integer(n)`)  
  An vector containing the flow ids of the collection.

- `data_ids`:

  (`integer(n)`)  
  An vector containing the data ids of the collection.

- `run_ids`:

  (`integer(n)`)  
  An vector containing the run ids of the collection.

- `task_ids`:

  (`integer(n)`)  
  An vector containing the task ids of the collection.

## Methods

### Public methods

- [`OMLCollection$new()`](#method-OMLCollection-new)

- [`OMLCollection$print()`](#method-OMLCollection-print)

- [`OMLCollection$download()`](#method-OMLCollection-download)

- [`OMLCollection$clone()`](#method-OMLCollection-clone)

Inherited methods

- [`mlr3oml::OMLObject$help()`](https://mlr3oml.mlr-org.com/dev/reference/OMLObject.html#method-help)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    OMLCollection$new(id, test_server = test_server_default())

#### Arguments

- `id`:

  (`integer(1)`)  
  OpenML id for the object.

- `test_server`:

  (`character(1)`)  
  Whether to use the OpenML test server or public server. Defaults to
  value of option `"mlr3oml.test_server"`, or `FALSE` if not set.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints the object.

#### Usage

    OMLCollection$print()

------------------------------------------------------------------------

### Method `download()`

Downloads the whole object for offline usage.

#### Usage

    OMLCollection$download()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OMLCollection$clone(deep = FALSE)

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
