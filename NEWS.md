# mlr3oml 0.12.0

* Move from `qs` to `qs2` package for caching.
* Add `uploader` argument to `list_oml_data()`.

# mlr3oml 0.11.0

## New Features

* Added `list_oml_collections()` to list collections (run or task collections) on OpenML.

# mlr3oml 0.10.1

* Bugfix: provide anchors for links in Rd files

# mlr3oml 0.10.0

* Bugfix: Caching of parquet files failed when the temporary directory was on a
different partition as the cache directory
* feat: Add option `mlr3oml.retries` to control number of retries when
downloading data from OpenML. The default is 3.

# mlr3oml 0.9.0

* Fix: Parquet datasets now work where columns simultaneously have to be renamed
and converted.
* Added upload functions:
  * `publish_data` to upload a dataset on OpenML
  * `publish_task` to create a task on OpenML
  * `publish_collection` to create a collection on OpenML

# mlr3oml 0.8.0

* Listing functions don't return the tables invisibly anymore.
* Address CRAN NOTE regarding unused bit64 import.
* Improved the printer for all OpenML objects.
* Removed `benchmark_grid_oml()`, which was already deprecated in release 0.7.2.
* Removed the fields `runs`, `flows`, `data`, `tasks` from the `OMLCollection` class.
  Consequently, the `cache` option can no longer be set for `OMLCollection` objects,
  see the class documentation for more information.
* Removed the examples, as they caused problems with CRAN checks when OpenML was unavailable.
* Caching can no longer be specified at the instance level but only globally through
  the option `mlr3oml.cache`
* Added `$download()` method for all OML objects to fully download an object for offline usage.
* Incremented the cache version for parquet data due to a change in OpenML.
* Added an online tutorial for the package.

# mlr3oml 0.7.2

* Fix: target is added to features when converting a `OMLData` object to a task with an explicit target variable that is **not** the default target.
* Deprecated `benchmark_grid_oml()` in favour of `mlr3::benchmark_grid(..., paired = TRUE)`
* Fix: Incremented cache version for data objects for int64 data types (introduced in the previous
  release).
* Fix: Incremented cache version for data description and fixed bug, as `make.names()` was not
  applied to ignore attributes.
* Fix bug in task converter (features were sometimes not set correctly)
* Collection now shows name in printer
* Better error message when parquet dataset creation fails

# mlr3oml 0.7.1

* Fixed argument names of S3 method for `as_data_backend` to comply with new CRAN checks

# mlr3oml 0.7.0

* feature: Add argument `task_type` to function `list_oml_tasks()`.
* fix: strings and nominals are distinguished for parquet files
* docs: Fixed some OpenML links
* docs: Renamed the docs for OpenML objects
* Renamed the sugar functions from:
  * `oml_data()` is now `odt()`
  * `oml_task() `is now `otsk()`
  * `oml_flow()` is now `oflw()`
  * `oml_run()` is now `orn`
  * `oml_collection()` is now `ocl()`
* Addresses a CRAN issue: examples fail gracefully if OpenML server is busy.


# mlr3oml 0.6.0

**Features**

* Add R6 classes for `OMLCollection`, `OMLRun`, `OMLFlow`.
* Added function `benchmark_grid_oml` that allows for easier creation of
  benchmark designs from OpenML task-resampling pairs.
* Added sugar functions `oml_flow`, `oml_data`, `oml_task`, `oml_run`,
  `oml_collection` for all OpenML objects.
* Conversion from OpenML to mlr3 objects is now only possible with the usual
  s3-converters `as_<object>`. This improves consistency by ensuring that the
  subcomponents of OpenML objects are always OpenML objects and not suddenly
  mlr3 objects.
* Added more converter functions: `as_learner`, `as_resample_result`,
  `as_data_backend`, `as_benchmark_result`.
* Added support for parquet files that were recently introduced on OpenML.
  The global option `mlr3oml.parquet` can be used to enable or disable this.
  By default it is `FALSE`. This is implemented via the duckdb backend from
  `mlr3db`.
* Support to use the OpenML test server. This can be globally enabled using the
  option `mlr3oml.test_server` or individually for objects.
  Options to globally define an API-key for the test server are through the
  environment variable `TESTOPENMLAPIKEY` or the option `mlr3oml.test_api_key`

**Fixes**

* Removed support for survival tasks as mlr3proba is no longer on CRAN
* OpenML tasks can now also be filtered according to the task type

**Other**

* Implement an arff writer and remove the arff dependency, therefore also
  removing the option `"farff"` as the `mlr3oml.arff_parser`
* Increment the cache version number due to changes in the cache structure: This
  will flush the previous cache folder.
* Simplified the code structure by adding `OMLObject` class from which all other
  OpenML objects like `OMLData`, `OMLTask` inherit.

# mlr3oml 0.5.0

* Support for downloading survival tasks (via `mlr3proba`).
* More functions to list objects from OpenML:
  - `list_oml_evaluations()`
  - `list_oml_flows()`
  - `list_oml_measures()`
  - `list_oml_runs()`
  - `list_oml_setups()`

# mlr3oml 0.4.3

* Fixed a bug regarding unquoting fields in ARFF files.

# mlr3oml 0.4.2

* If not set via option `mlr3oml.api_key`, the API key is retrieved from the
  environment variable `OPENMLAPIKEY`.
* Implemented a retry mechanism as a workaround for temporary connection errors.

# mlr3oml 0.4.1

* Added a heuristic to detect the quote char.

# mlr3oml 0.4.0

* The parsers for ARFF files can now be explicitly selected via option
  `"mlr3oml.arff.parser"`. Default is the internal parser based on
  `data.table::fread()`.
* Improved stability of the internal ARFF parser in case of malformed ARFF
  files and non-standardized quotes.

# mlr3oml 0.3.0

* The connectors used in `mlr_tasks` and `mlr_resamplings` now signal errors of
  class `missingDefaultError` if some defaults are not set.
* Target columns are now automatically converted to the require storage mode
  during task creation.
* Removed dependency on orphaned package `bibtex`.

# mlr3oml 0.2.0

* Support filtering data sets and tasks via data id or task id (#5).
* Added fallback to RWeka for sparse ARFF files (#6).
* Fixed import from backports.

# mlr3oml 0.1.0

* Initial release.
