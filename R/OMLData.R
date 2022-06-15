#' @title Interface to OpenML Data Sets
#'
#' @description
#' This is the class for data sets served on [OpenML](https://openml.org/search?type=data&sort=runs&status=active).
#'
#' @section mlr3 Integration:
#' * A [mlr3::Task] can be obtained by calling `as_task()`.
#' * A [mlr3::DataBackend] can be obtained by calling `as_data_backend()`.
#'
#' @section ARFF Files:
#' This package comes with an own reader for ARFF files, based on [data.table::fread()].
#' For sparse ARFF files and if the \CRANpkg{RWeka} package is installed, the reader
#' automatically falls back to the implementation in ([RWeka::read.arff()]).
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
#'
#' # get a task via tsk():
#' tsk("oml", data_id = 9)
#' }
OMLData = R6Class("OMLData",
  public = list(
    #' @field id (`integer(1)`)\cr
    #' OpenML data id.
    id = NULL,
    #' @template field_cache_dir
    cache_dir = NULL,
    #' @field parquet (`logical(1)`)\cr
    #' Whether to use parquet.
    parquet = TRUE,
    #' @description
    #' Creates a new object of class `OMLData`.
    #'
    #' @param id (`integer(1)`)\cr
    #'   OpenML data id.
    #' @template param_cache
    #' @param parquet (`logical(1)`)\cr
    #'   Whether to use parquet instead of arff.
    initialize = function(id, cache = getOption("mlr3oml.cache", FALSE),
      parquet = getOption("mlr3oml.parquet", FALSE)) {
      self$id = assert_count(id, coerce = TRUE)
      self$cache_dir = get_cache_dir(cache)
      self$parquet = assert_flag(parquet)
      initialize_cache(self$cache_dir)
    },
    #' @description
    #' Prints the object.
    #' For a more detailed printer, convert to a [mlr3::Task] via `as_task()`.
    print = function() {
      catf("<OMLData:%i:%s> (%ix%i)", self$id, self$name, self$nrow, self$ncol)
      catf(" * Default target: %s", self$target_names)
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
    #' @field name (`character(1)`)\cr
    #' Name of the data set, as extracted from the data set description.
    name = function() {
      self$desc$name
    },
    #' @field desc (`list()`)\cr
    #' Data set description (meta information), downloaded and converted from the JSON API response.
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = cached(download_data_desc, "data_desc", self$id, cache_dir = self$cache_dir)
      }
      private$.desc
    },
    #' @field qualities (`data.table()`)\cr
    #' Data set qualities (performance values), downloaded from the JSON API response and
    #' converted to a [data.table::data.table()] with columns `"name"` and `"value"`.
    qualities = function() {
      if (is.null(private$.qualities)) {
        private$.qualities = cached(download_data_qualities, "data_qualities", self$id,
          cache_dir = self$cache_dir)
      }
      private$.qualities
    },
    #' Returns the data without the row identifier and ignore id columns.
    #' @field data (`data.table()`)\cr
    #' Returns the data without the row id and ignore columns.
    #' For the complete data, use `$data_raw`.
    data = function() {
      cols = !self$features$is_ignore & !self$features$is_row_identifier
      self$backend$data(self$backend$rownames, self$features$name[cols])
    },
    #' @field data_raw (`data.table()`)\cr
    #' Returns the complete data.
    data_raw = function() {
      if (self$parquet) {
        stopf("Currently, the parquet files are missing the id column and ignore columns.")
      }
      self$backend$data(self$backend$rownames, self$features$name)
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
          desc = self$desc, cache_dir = self$cache_dir
        )
      }
      private$.features
    },
    #' @field backend (`mlr3::DataBackend`)\cr
    #'   The data backend.
    backend = function() {
      if (!is.null(private$.backend)) {
        return(private$.backend)
      }

      if (self$parquet) {
        path = self$parquet_path
        private$.backend = mlr3db::as_duckdb_backend(path)
      } else {
        data = cached(download_data, "data", self$id, desc = self$desc, cache_dir = self$cache_dir)
        private$.backend = as_data_backend(data)
      }

      private$.backend
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
    #' @field tags (`character()`)\cr
    #' Returns all tags of the data set.
    tags = function() {
      self$desc$tag
    },
    #' @field license (`character()`)\cr
    #' Returns all license of the dataset.
    license = function() {
      self$desc$licence
    },
    #' @field parquet_path (`character()`)\cr
    #' Returns the path of the parquet file.
    parquet_path = function() {
      if (isFALSE(self$parquet)) {
        messagef("Parquet is not the selected data format, returning NULL.")
        return(NULL)
      }
      if (is.null(private$.data_path)) {
        loadNamespace("mlr3db")
        # this function is already cached, it works a little different than the cached(f, ...)
        # because we cache it as parquet and not .qs
        private$.parquet_path = download_parquet(
          url = self$desc$minio_url,
          id = self$id,
          cache_dir = self$cache_dir,
          api_key = get_api_key()
        )
      }
      private$.parquet_path
    }
  ),
  private = list(
    .desc = NULL,
    .qualities = NULL,
    .features = NULL,
    .backend = NULL,
    .parquet_path = NULL
  )
)

#' @export
as_data_backend.OMLData = function(data, primary_key = NULL, ...) {
  if (data$parquet) {
    loadNamespace("mlr3db")
    backend = as_duckdb_backend(data$parquet_path, primary_key = primary_key, ...)
  } else {
    backend = as_data_backend(data$data, primary_key = primary_key, ...)
  }
}

#' @export
as_data_backend.OMLTask = function(data, primary_key = NULL, ...) {
  as_data_backend(data$data, primary_key = primary_key, ...)
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
      "nominal" = new_task_classif,
      "numeric" = new_task_regr,
      NULL
    )
  } else if (length(target) == 2L) {
    stopf("mlr3proba currently not supported.")
    # constructor = new_task_surv
  }
  if (is.null(constructor)) {
    stopf("Unable to determine the task type")
  }
  constructor(x$name, x$data, target = target)
}
