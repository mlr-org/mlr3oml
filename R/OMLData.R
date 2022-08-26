#' @title Interface to OpenML Data Sets
#'
#' @name oml_data
#'
#' @description
#' This is the class for data sets served on [OpenML](https://openml.org/search?type=data&sort=runs&status=active).
#'
#' @section mlr3 Integration:
#' * A [mlr3::Task] can be obtained by calling `as_task()`.
#' * A [mlr3::DataBackend] can be obtained by calling `as_data_backend()`. Depending on the
#'   selected file-type, the returned backend is a `DataBackendDataTable` (arff) or
#'   `DataBackendDuckDB` (parquet).
#'
#' @section Name conversion:
#' Note that we rename the columns to comply with R's naming scheme (see [base::make.names()]).
#' This means that the names can differ from those on OpenML.
#'
#' @section File Format:
#' The datasets stored on OpenML are either stored as (sparse) ARFF or parquet.
#' When creating a new `OMLData` object, the constructor argument `parquet` allows to switch
#' between arff and parquet. Note that not necessarily all data files are available as parquet.
#' The option `mlr3oml.parquet` can be used to set a default.
#'
#' @section ARFF Files:
#' This package comes with an own reader for ARFF files, based on [data.table::fread()].
#' For sparse ARFF files and if the \CRANpkg{RWeka} package is installed, the reader
#' automatically falls back to the implementation in ([RWeka::read.arff()]).
#'
#' @section Parquet Files:
#' For the handling of parquet files, we rely on \CRANpkg{duckdb}.
#'
#' @references
#' `r format_bib("vanschoren2014")`
#'
#' @export
#' @examples
#' library("mlr3")
#' \donttest{
#' # OpenML Data object
#' odata = OMLData$new(id = 9)
#' print(odata)
#' print(odata$target_names)
#' print(odata$feature_names)
#' print(odata$tags)
#'
#' # mlr3 conversion:
#' task = as_task(odata)
#' backend = as_data_backend(odata)
#' class(backend)
#'
#' # get a task via tsk():
#' tsk("oml", data_id = 9)
#'
#' # For parquet files
#' if (requireNamespace("duckdb")) {
#'   odata = OMLData$new(id = 9, parquet = TRUE)
#'
#'   print(odata)
#'   print(odata$target_names)
#'   print(odata$feature_names)
#'   print(odata$tags)
#'
#'   backend = as_data_backend(odata)
#'   class(backend)
#'   task = as_task(odata)
#'   task = tsk("oml", data_id = 9, parquet = TRUE)
#'   class(task$backend)
#' }
#' }
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
      cache = getOption("mlr3oml.cache", FALSE),
      parquet = getOption("mlr3oml.parquet", FALSE),
      test_server = getOption("mlr3oml.test_server", FALSE)
      ) {
      super$initialize(id, cache, parquet, test_server, "data")
    },
    #' @description
    #' Prints the object.
    #' For a more detailed printer, convert to a [mlr3::Task] via `as_task()`.
    print = function() {
      catf("<OMLData:%i:%s> (%ix%i)", self$id, as_short_string(self$name), self$nrow, self$ncol)
      catf(" * Default target: %s", as_short_string(self$target_names))
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
    #' @field data (`data.table()`)\cr
    #' Returns the data (without the row identifier and ignore id columns).
    data = function() {
      cols = !self$features$is_ignore & !self$features$is_row_identifier
      backend = private$.get_backend()
      backend$data(backend$rownames, self$features$name[cols])
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
    .get_backend = function() {
      if (!is.null(private$.backend)) {
        return(private$.backend)
      }
      if (self$parquet) {
        path = self$parquet_path
        backend = mlr3db::as_duckdb_backend(path)
        if (!test_names(backend$colnames, type = "strict")) {
          new = make.names(backend$colnames)
          if (length(unique(new)) != length(new)) {
            stopf("No unique names after conversion.")
          }
          # This code is a little hacky. The reason is that DataBackendRename is not exported
          # in mlr3 and only accessible via DataBackendRename (mlr3 version 0.14).
          # This can be changed when it is exported in mlr3.
          private$.backend = withr::with_options(list(mlr3.allow_utf8_names = TRUE),
            Task$new("temp", "regr", backend)$rename(backend$colnames, new)$backend
          )
        } else {
          private$.backend = backend
        }
      } else {
        data = cached(download_arff, "data", self$id, desc = self$desc, cache_dir = self$cache_dir,
          server = self$server, test_server = self$test_server
        )
        private$.backend = as_data_backend(data)
      }

      private$.backend
    }
  )
)

#' @export
as_data_backend.OMLData = function(data, primary_key = NULL, ...) {
  if (data$parquet) {
    loadNamespace("mlr3db")
    mlr3db::as_duckdb_backend(data$parquet_path, primary_key = primary_key, ...)
  } else {
    as_data_backend(data$data, primary_key = primary_key, ...)
  }
}

#' @importFrom mlr3 as_task
#' @export
as_task.OMLData = function(x, target_names = NULL, ...) {
  target = target_names %??% x$target_names
  assert_subset(target, c(x$target_names, x$feature_names))
  if (length(target) == 0L) {
    stopf("Data set with id '%i' does not have a default target attribute", x$id)
  }
  constructor = NULL
  if (length(target) == 1L) {
    constructor = switch(as.character(x$features[list(target), "data_type", on = "name", with = FALSE][[1L]]),
      "nominal" = TaskClassif,
      "numeric" = TaskRegr,
      NULL
    )
  } else if (length(target) == 2L) {
    stopf("mlr3proba currently not supported.")
  }
  if (is.null(constructor)) {
    stopf("Unable to determine the task type")
  }
  constructor$new(x$name, get_private(x)$.get_backend(), target = target)
}


