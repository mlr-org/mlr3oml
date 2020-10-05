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
