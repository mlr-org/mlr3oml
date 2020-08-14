#' @title Interface to OpenML Data Sets
#'
#' @description
#' This is the class for data sets served on \url{https://openml.org/d}.
#'
#' @section mlr3 Integration:
#' A [mlr3::Task] is returned by the method `$task`.
#' Alternatively, you can convert this object to a [mlr3::DataBackend] using
#' `mlr3::as_data_backend()`.
#'
#' @section ARFF Files:
#' This package comes with an own reader for ARFF files, based on [data.table::fread()].
#' For sparse ARFF files and if the \CRANpkg{RWeka} package is installed, the reader
#' automatically falls back to the implementation in ([RWeka::read.arff()]).
#'
#' @references
#' \cite{mlr3oml}{vanschoren2014}
#'
#' @export
#' @examples
#' \donttest{
#' odata = OMLData$new(id = 9)
#'
#' print(odata)
#' print(odata$target_names)
#' print(odata$feature_names)
#' print(odata$tags)
#' print(odata$task())
#'
#' # get a task via tsk():
#' if (requireNamespace("mlr3")) {
#'   mlr3::tsk("oml", data_id = 9)
#' }
#' }
OMLData = R6Class("OMLData",
  public = list(

    #' @field id (`integer(1)`)\cr
    #' OpenML data id.
    id = NULL,

    #' @template field_cache_dir
    cache_dir = NULL,

    #' @description
    #' Creates a new object of class `OMLData`.
    #'
    #' @param id (`integer(1)`)\cr
    #'   OpenML data id.
    #' @template param_cache
    initialize = function(id, cache = getOption("mlr3oml.cache", FALSE)) {
      self$id = assert_count(id, coerce = TRUE)
      self$cache_dir = get_cache_dir(cache)
      initialize_cache(self$cache_dir)
    },

    #' @description
    #' Prints the object.
    #' For a more detailed printer, convert to a [mlr3::Task] via `$task()`.
    print = function() {
      catf("<OMLData:%i:%s> (%ix%i)", self$id, self$name, self$nrow, self$ncol)
    },

    #' @description
    #' Returns the value of a single OpenML data set quality.
    #'
    #' @param name (`character(1)`)\cr
    #'   Name of the quality to extract.
    quality = function(name) {
      id = assert_string(name)
      self$qualities[list(id), "value", on = "name", with = FALSE][[1L]]
    },

    #' @description
    #' Creates a [mlr3::Task] using the provided target column, defaulting to the default target attribute
    #' of the task description.
    #'
    #' @param target_names (`character()`)\cr
    #'   Name(s) of the target columns, or `NULL` for the default columns.
    task = function(target_names = NULL) {
      target = target_names %??% self$target_names
      if (length(target) == 0L) {
        stopf("Data set with id '%i' does not have a default target attribute", self$id)
      }

      switch(as.character(self$features[list(target), "data_type", on = "name", with = FALSE][[1L]]),
        "nominal" = TaskClassif$new(self$name, self$data, target = target),
        "numeric" = TaskRegr$new(self$name, self$data, target = target),
        stop("Unknown task type")
      )
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
        private$.qualities = cached(download_data_qualities, "data_qualities", self$id, cache_dir = self$cache_dir)
      }
      private$.qualities
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
          desc = self$desc, cache_dir = self$cache_dir)
      }

      private$.features
    },

    #' @field data (`data.table()`)\cr
    #' Data as [data.table::data.table()].
    #' Columns marked as row identifiers or marked with the ignore flag are automatically removed.
    data = function() {
      if (is.null(private$.data)) {
        private$.data = cached(download_data, "data", self$id, desc = self$desc, cache_dir = self$cache_dir)
      }

      private$.data
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
    }
  ),

  private = list(
    .data = NULL,
    .desc = NULL,
    .qualities = NULL,
    .features = NULL
  )
)
