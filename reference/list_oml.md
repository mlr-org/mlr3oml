# List Data from OpenML

This function allows to query data sets, tasks, flows, setups, runs, and
evaluation measures from
<https://www.openml.org/search?type=data&sort=runs&status=active> using
some simple filter criteria.

To find datasets for a specific task type, use `list_oml_tasks()` which
supports filtering according to the task type. Another heuristic to
search for possible regression tasks is to search for data sets with 0
number of classes, i.e. by specifying `number_classes = 0`.

## Usage

``` r
list_oml_collections(
  uploader = NULL,
  status = "all",
  main_entity_type = NULL,
  limit = limit_default(),
  test_server = test_server_default(),
  ...
)

list_oml_data(
  data_id = NULL,
  data_name = NULL,
  number_instances = NULL,
  number_features = NULL,
  number_classes = NULL,
  number_missing_values = NULL,
  tag = NULL,
  uploader = NULL,
  limit = limit_default(),
  test_server = test_server_default(),
  ...
)

list_oml_evaluations(
  run_id = NULL,
  task_id = NULL,
  measures = NULL,
  tag = NULL,
  limit = limit_default(),
  test_server = test_server_default(),
  ...
)

list_oml_flows(
  uploader = NULL,
  tag = NULL,
  limit = limit_default(),
  test_server = test_server_default(),
  ...
)

list_oml_measures(test_server = test_server_default())

list_oml_runs(
  run_id = NULL,
  task_id = NULL,
  tag = NULL,
  flow_id = NULL,
  limit = limit_default(),
  test_server = test_server_default(),
  ...
)

list_oml_setups(
  flow_id = NULL,
  setup_id = NULL,
  tag = NULL,
  limit = limit_default(),
  test_server = test_server_default(),
  ...
)

list_oml_tasks(
  task_id = NULL,
  data_id = NULL,
  number_instances = NULL,
  number_features = NULL,
  number_classes = NULL,
  number_missing_values = NULL,
  tag = NULL,
  limit = limit_default(),
  test_server = test_server_default(),
  type = NULL,
  ...
)
```

## Arguments

- uploader:

  (`integer(1)`)  
  Filter for uploader.

- status:

  (`character(1)`)  
  Should be one of "active", "in_preparation", "deactivated", "all". By
  default "all" studies are returned.

- main_entity_type:

  (`character(1)` \| `NULL`)  
  Filter for main entity type. Can be "run" or "task".

- limit:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Limit the results to `limit` records. Default is the value of option
  `"mlr3oml.limit"`, defaulting to 5000.

- test_server:

  (`character(1)`)  
  Whether to use the OpenML test server or public server. Defaults to
  value of option `"mlr3oml.test_server"`, or `FALSE` if not set.

- ...:

  (any)  
  Additional (unsupported) filters, as named arguments.

- data_id:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Vector of data ids to restrict to.

- data_name:

  (`character(1)`)  
  Filter for name of data set.

- number_instances:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Filter for number of instances.

- number_features:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Filter for number of features.

- number_classes:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Filter for number of labels of the target (only classification tasks).

- number_missing_values:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Filter for number of missing values.

- tag:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Filter for tags. You can provide multiple tags as character vector.

- run_id:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Vector of run ids to restrict to.

- task_id:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Vector of task ids to restrict to.

- measures:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Vector of evaluation measures to restrict to.

- flow_id:

  (`integer(1)`)  
  Filter for flow id.

- setup_id:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Vector of setup ids to restrict to.

- type:

  (`character(1)`)  
  The task type, supported values are: `"clasisf"`, `"regr"`, `"surv"`
  and `"clust"`.

## Value

([`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))
of results, or a null data.table if no data set matches the filter
criteria.

## Details

Filter values are usually provided as single atomic values (typically
integer or character). Provide a numeric vector of length 2 (`c(l, u)`)
to find matches in the range \\\[l, u\]\\.

Note that only a subset of filters is exposed here. For a more
feature-complete package, see
[OpenML](https://CRAN.R-project.org/package=OpenML). Alternatively, you
can pass additional filters via `...` using the names of the official
API, c.f. the *REST* tab of <https://www.openml.org/apis>.

## References

Casalicchio G, Bossek J, Lang M, Kirchhoff D, Kerschke P, Hofner B,
Seibold H, Vanschoren J, Bischl B (2017). “OpenML: An R Package to
Connect to the Machine Learning Platform OpenML.” *Computational
Statistics*, 1–15.
[doi:10.1007/s00180-017-0742-2](https://doi.org/10.1007/s00180-017-0742-2)
.

Vanschoren J, van Rijn JN, Bischl B, Torgo L (2014). “OpenML.” *ACM
SIGKDD Explorations Newsletter*, **15**(2), 49–60.
[doi:10.1145/2641190.2641198](https://doi.org/10.1145/2641190.2641198) .

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
