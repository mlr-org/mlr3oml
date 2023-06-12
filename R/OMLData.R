#' @title Interface to OpenML Data Sets
#'
#' @name oml_data
#'
#' @description
#' This is the class for data sets served on [OpenML](https://www.openml.org/search?type=data&status=active).
#' This object can also be constructed using the sugar function [odt()].
#'
#' @section mlr3 Integration:
#' * A [mlr3::Task] can be obtained by calling [mlr3::as_task()].
#'   The target column must either be the default target (this is the default behaviour) or one of `$feature_names`.
#'   In case the target is specified to be one of `$feature_names`, the default target is added to the features
#'   of the task.
#' * A [mlr3::DataBackend] can be obtained by calling [mlr3::as_data_backend()]. Depending on the
#'   selected file-type, the returned backend is a [mlr3::DataBackendDataTable] (arff) or
#'   [mlr3db::DataBackendDuckDB] (parquet).
#'   Note that a converted backend can contain columns beyond the target and the features (id column or ignore columns).
#'
#' @section Name conversion:
#' Column names that don't comply with R's naming scheme are renamed (see [base::make.names()]).
#' This means that the names can differ from those on OpenML.
#'
#' @section File Format:
#' The datasets stored on OpenML are either stored as (sparse) ARFF or parquet.
#' When creating a new `OMLData` object, the constructor argument `parquet` allows to switch
#' between arff and parquet. Note that not necessarily all data files are available as parquet.
#' The option `mlr3oml.parquet` can be used to set a default.
#' If `parquet` is `TRUE` but not available, `"arff"` will be used as a fallback.
#'
#' @section ARFF Files:
#' This package comes with an own reader for ARFF files, based on [data.table::fread()].
#' For sparse ARFF files and if the \CRANpkg{RWeka} package is installed, the reader
#' automatically falls back to the implementation in ([RWeka::read.arff()]).
#'
#' @section Parquet Files:
#' For the handling of parquet files, we rely on \CRANpkg{duckdb} and `CRANpkg{DBI}`.
#'
#' @references
#' `r format_bib("vanschoren2014")`
#'
#' @export
#' @examples
#' try({
#'   library("mlr3")
#'   # OpenML Data object
#'   odata = OMLData$new(id = 9)
#'   # using sugar
#'   odata = odt(id = 9)
#'   print(odata)
#'   print(odata$target_names)
#'   print(odata$feature_names)
#'   print(odata$tags)
#'
#'   # mlr3 conversion:
#'   task = as_task(odata)
#'   backend = as_data_backend(odata)
#'   class(backend)
#'
#'   # get a task via tsk():
#'   tsk("oml", data_id = 9)
#'
#'   # For parquet files
#'   if (requireNamespace("duckdb")) {
#'     odata = OMLData$new(id = 9, parquet = TRUE)
#'     # using sugar
#'     odata = odt(id = 9)
#'
#'     print(odata)
#'     print(odata$target_names)
#'     print(odata$feature_names)
#'     print(odata$tags)
#'
#'     backend = as_data_backend(odata)
#'     class(backend)
#'     task = as_task(odata)
#'     task = tsk("oml", data_id = 9, parquet = TRUE)
#'     class(task$backend)
#'   }
#' }, silent = TRUE)
OMLData = R6Class("OMLData",
  inherit = OMLObject,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_id
    #' @template param_cache
    #' @template param_parquet
    #' @template param_test_server
    initialize = function(
      id,
      cache = cache_default(),
      parquet = parquet_default(),
      test_server = test_server_default()
      ) {
      private$.parquet = assert_flag(parquet)
      super$initialize(id, cache, test_server, "data")
    },
    #' @description
    #' Prints the object.
    #' For a more detailed printer, convert to a [mlr3::Task] via `as_task()`.
    print = function() {
      catf("<OMLData:%i:%s> (%ix%i)", self$id, as_short_string(self$name), self$nrow, self$ncol)
      dt = if (length(self$target_names)) as_short_string(self$target_names) else "<none>"
      catf(" * Default target: %s", dt)
      if (self$test_server) {
        catf(" * Using test server")
      }
    },
    #' @description
    #' Returns the value of a single OpenML data set quality.
    #'
    #' @param name (`character(1)`)\cr
    #'   Name of the quality to extract.
    quality = function(name) {
      id = assert_string(name)
      self$qualities[list(id), "value", on = "name", with = FALSE][[1L]]
    }
  ),
  active = list(
    #' @field qualities (`data.table()`)\cr
    #' Data set qualities (performance values), downloaded from the JSON API response and
    #' converted to a [data.table::data.table()] with columns `"name"` and `"value"`.
    qualities = function() {
      if (is.null(private$.qualities)) {
        private$.qualities = cached(download_data_qualities, "data_qualities", self$id,
          cache_dir = self$cache_dir, server = self$server, test_server = self$test_server)
      }
      private$.qualities
    },
    #' @field tags (`character()`)\cr
    #' Returns all tags of the object.
    tags = function() {
      self$desc$tag
    },
    #' @field parquet (`logical(1)`)\cr
    #' Whether to use parquet.
    parquet = function(rhs) {
      assert_ro_binding(rhs)
      private$.parquet
    },
    #' @field data (`data.table()`)\cr
    #' Returns the data (without the row identifier and ignore id columns).
    data = function() {
      backend = private$.get_backend()
      ii = !self$features$is_ignore & !self$features$is_row_identifier
      cols = self$features$name[ii]
      existing = setdiff(backend$colnames, backend$primary_key)
      if (!test_subset(cols, existing)) {
        missing = setdiff(cols, existing)
        warningf("Data is missing features from feature description {%s}.\n", paste0(missing, collapse = ", "))
      }
      backend$data(backend$rownames, cols)
    },
    #' @field features (`data.table()`)\cr
    #' Information about data set features (including target), downloaded from the JSON API response and
    #'   converted to a [data.table::data.table()] with columns:
    #'   * `"index"` (`integer()`): Column position.
    #'   * `"name"` (`character()`): Name of the feature.
    #'   * `"data_type"` (`factor()`): Type of the feature: `"nominal"` or `"numeric"`.
    #'   * `"nominal_value"` (`list()`): Levels of the feature, or `NULL` for numeric features.
    #'   * `"is_target"` (`logical()`): `TRUE` for target column, `FALSE` otherwise.
    #'   * `"is_ignore"` (`logical()`): `TRUE` if this feature should be ignored.
    #'     Ignored features are removed automatically from the data set.
    #'   * `"is_row_identifier"` (`logical()`): `TRUE` if the column encodes a row identifier.
    #'     Row identifiers are removed automatically from the data set.
    #'   * `"number_of_missing_values"` (`integer()`): Number of missing values in the column.
    features = function() {
      if (is.null(private$.features)) {
        private$.features = cached(download_data_features, "data_features", self$id,
          desc = self$desc, cache_dir = self$cache_dir, server = self$server,
          test_server = self$test_server
        )
      }
      private$.features
    },
    #' @field target_names (`character()`)\cr
    #' Name of the default target, as extracted from the OpenML data set description.
    target_names = function() {
      self$desc$default_target_attribute
    },
    #' @field feature_names (`character()`)\cr
    #' Name of the features, as extracted from the OpenML data set description.
    feature_names = function() {
      self$features[!is_target & !is_ignore & !is_row_identifier, "name", with = FALSE][[1L]]
    },
    #' @field nrow (`integer()`)\cr
    #' Number of observations, as extracted from the OpenML data set qualities.
    nrow = function() {
      as.integer(self$quality("NumberOfInstances"))
    },
    #' @field ncol (`integer()`)\cr
    #' Number of features (including targets), as extracted from the table of data set features.
    #' This excludes row identifiers and ignored columns.
    ncol = function() {
      self$features[!is_row_identifier & !is_ignore, .N]
    },
    #' @field license (`character()`)\cr
    #' Returns all license of the dataset.
    license = function() {
      self$desc$licence
    },
    #' @field parquet_path (`character()`)\cr
    #' Downloads the parquet file (or loads from cache) and returns the path of the parquet file.
    #' Note that this also normalizes the names of the parquet file.
    parquet_path = function() {
      if (isFALSE(self$parquet)) {
        messagef("Parquet is not the selected data format, returning NULL.")
        return(NULL)
      }
      if (is.null(private$.parquet_path)) {
        loadNamespace("mlr3db")
        # this function is already cached, it works a little different than the cached(f, ...)
        # because we cache it as .parquet and not as .qs
        private$.parquet_path = cached(
          download_parquet, "data_parquet", self$id, desc = self$desc, cache_dir = self$cache_dir,
          server = self$server, test_server = self$test_server, parquet = TRUE
        )
      }
      private$.parquet_path
    }
  ),
  private = list(
    .qualities = NULL,
    .features = NULL,
    .backend = NULL,
    .parquet_path = NULL,
    .parquet = NULL,
    .get_backend = function(primary_key = NULL) {
      if (!is.null(private$.backend)) {
        return(private$.backend)
      }
      backend = NULL
      if (self$parquet) {
        require_namespaces(c("mlr3db", "duckdb", "DBI"))
        path = try({self$parquet_path}, silent = TRUE)
        if (inherits(path, "try-error")) {
          lg$info("Failed to download parquet, trying arff.", id = self$id)
        } else {
          factors = self$features[get("data_type") == "nominal", "name"][[1L]]
          backend = try(as_duckdb_backend_character(path, primary_key = primary_key, factors = factors), silent = TRUE)
          if (inherits(backend, "try-error")) {
            msg = sprintf(
              "Parquet available but failed to create backend, reverting to arff. Error message is '%s'", # nolint
              backend
            )
            lg$info(msg, id = self$id)
          }
        }
      }
      if (is.null(backend) || inherits(path, "try-error") || inherits(backend, "try-error")) {
        data = cached(download_arff, "data", self$id, desc = self$desc, cache_dir = self$cache_dir,
          server = self$server, test_server = self$test_server
        )
        backend = as_data_backend(data, primary_key = primary_key)
      }
      private$.backend = backend
    }
  )
)

#' @export
as_data_backend.OMLData = function(data, primary_key = NULL, ...) {
  get_private(data)$.get_backend(primary_key)
}

#' @importFrom mlr3 as_task
#' @export
as_task.OMLData = function(x, target_names = NULL, ...) {
  # maybe we could also allow ignore columns here
  target = target_names %??% x$target_names
  if (is.null(target) || !length(target)) {
    stopf("Either a default target must be available or argument 'target_names' must be specified.")
  } else if (!test_subset(target, c(x$target_names, x$feature_names))) {
    stopf("Target '%s' not found in target names or feature names.", target)
  }

  if (length(target) == 1L) {
    constructor = switch(as.character(x$features[list(target), "data_type", on = "name", with = FALSE][[1L]]),
      "nominal" = TaskClassif,
      "numeric" = TaskRegr,
      stopf("Unable to determine the task type")
    )
  } else if (length(target) == 2L) {
    stopf("mlr3proba currently not supported.")
  }
  task = constructor$new(x$name, as_data_backend(x), target = target)

  if (!length(x$target_names)) {
    task$col_roles$feature = setdiff(x$feature_names, target)
  } else if (x$target_names == target) {
    task$col_roles$feature = x$feature_names
  } else {
    # In case default target exists but we use another target, we add the default target
    # to the features
    task$col_roles$feature = setdiff(c(x$feature_names, x$target_names), target)
  }
  return(task)
}

