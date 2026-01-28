# mlr3oml: Connector Between 'mlr3' and 'OpenML'

Provides an interface to 'OpenML.org' to list and download machine
learning data, tasks and experiments. The 'OpenML' objects can be
automatically converted to 'mlr3' objects. For a more sophisticated
interface with more upload options, see the 'OpenML' package.

## Documentation

Start by reading the Large-Scale Benchmarking
[chapter](https://mlr3book.mlr-org.com/chapters/chapter11/large-scale_benchmarking.html)
from the mlr3book.

## mlr3 Integration

This package adds the
[mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) `"oml"` and
the
[mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)
`"oml"` to
[mlr3::mlr_tasks](https://mlr3.mlr-org.com/reference/mlr_tasks.html) and
[mlr3::mlr_resamplings](https://mlr3.mlr-org.com/reference/mlr_resamplings.html),
respectively. For the former you may pass either a `data_id` or a
`task_id`, the latter requires a `task_id`. Furthermore it allows to
convert the OpenML objects to mlr3 objects using the usual S3 generics
such as
[mlr3::as_task](https://mlr3.mlr-org.com/reference/as_task.html),
[mlr3::as_learner](https://mlr3.mlr-org.com/reference/as_learner.html),
[mlr3::as_resampling](https://mlr3.mlr-org.com/reference/as_resampling.html),
[mlr3::as_resample_result](https://mlr3.mlr-org.com/reference/as_resample_result.html),
[mlr3::as_benchmark_result](https://mlr3.mlr-org.com/reference/as_benchmark_result.html)
or
[mlr3::as_data_backend](https://mlr3.mlr-org.com/reference/as_data_backend.html).
This allows for a frictionless integration of OpenML and mlr3.

## Options

- `mlr3oml.cache`: Enables or disables caching globally. If set to
  `FALSE`, caching is disabled. If set to `TRUE`, cache directory as
  reported by
  [`backports::R_user_dir()`](https://rdrr.io/pkg/backports/man/R_user_dir.html)
  is used. Alternatively, you can specify a path on the local file
  system here. Default is `FALSE`.

- `mlr3oml.api_key`: API key to use. All operations supported by this
  package work without an API key, but you might get rate limited
  without an API key. If not set, defaults to the value of the
  environment variable `OPENMLAPIKEY`.

- `mlr3oml.arff_parser`: ARFF parser to use, defaults to the internal
  one relies on
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html).
  Can also be set to `"RWeka"` for the parser in
  [RWeka](https://CRAN.R-project.org/package=RWeka).

- `mlr3oml.parquet`: Enables or disables parquet as the default file
  format. If set to `TRUE`, the parquet version of datasets will be used
  by default. If set to `FALSE`, the arff version of datasets will be
  used by default. Note that the OpenML sever is still transitioning
  from arff to parquet and some features will work better with arff.
  Default is `FALSE`.

- `mlr3oml.retries`: An integer defining number of retries when
  downloading data from OpenML. If it is `NULL`, the number of retries
  is set to 3.

**Relevant for developers**

- `mlr3oml.test_server`: The default value for whether to use the OpenML
  test server. Default is `FALSE`.

- `mlr3oml.test_api_key`: API key to use for the test server. If not
  set, defaults to the value of the environment variable
  `TESTOPENMLAPIKEY`.

## Logging

The [lgr](https://CRAN.R-project.org/package=lgr) package is used for
logging. To change the threshold, use
`lgr::get_logger("mlr3oml")$set_threshold()`.

## See also

Useful links:

- <https://mlr3oml.mlr-org.com>

- <https://github.com/mlr-org/mlr3oml>

- Report bugs at <https://github.com/mlr-org/mlr3oml/issues>

## Author

**Maintainer**: Sebastian Fischer <sebf.fischer@gmail.com>
([ORCID](https://orcid.org/0000-0002-9609-3197))

Authors:

- Michel Lang <michellang@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-9754-0393))
