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
