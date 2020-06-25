#' @title Interface to OpenML Data Sets
#'
#' @description
#' This is the class for data sets served on \url{https://openml.org/d}.
#'
#' @export
OMLData = R6Class("OMLData",
  public = list(

    #' @field id (`integer(1)`)\cr
    #' OpenML data id.
    id = NULL,

    #' @field use_cache (`logical(1)`)\cr
    #' If `TRUE`, locally caches downloaded objects on the file system.file.
    #' See [R_user_dir()] for the path (depending on your operating system).
    use_cache = NULL,

    #' @description
    #' Creates a new object of class `OMLData`.
    #'
    #' @param id (`integer(1)`)\cr
    #'   OpenML data id.
    #' @param use_cache (`logical(1)`)\cr
    #'   Flag to control the file system cache.
    #'   See [R_user_dir()] for the path (depending on your operating system).
    #'   Default is the value of option `"mlr3oml.use_cache"` or `FALSE`.
    initialize = function(id, use_cache = getOption("mlr3oml.use_cache", FALSE)) {
      self$id = assert_count(id, coerce = TRUE)
      self$use_cache = assert_flag(use_cache)
    },

    #' @description
    #' Returns the value of a single OpenML data set quality.
    #'
    #' @param name (`character(1)`)\cr
    #'   Name of the quality to extract.
    quality = function(name) {
      id = assert_string(name)
      self$qualities[.(id), value, on = "name"]
    }
  ),

  active = list(
    #' @field name (`character(1)`)\cr
    #' Name of the data set, as extracted from the data set description.
    name = function() {
      self$description$name
    },

    #' @field description (`list()`)\cr
    #' Data set description (meta information), downloaded and converted from the JSON API response.
    description = function() {
      if (is.null(private$.description)) {
        private$.description = cached(download_data_description, "data_description", self$id, use_cache = self$use_cache)
     }
      private$.description
    },

    #' @field qualities (`data.table()`)\cr
    #' Data set qualities (performance values), downloaded from the JSON API response and
    #' converted to a [data.table()`] with columns `"name"` and `"value"`.
    qualities = function() {
      if (is.null(private$.qualities)) {
        private$.qualities = cached(download_data_qualities, "data_qualities", self$id, use_cache = self$use_cache)
      }
      private$.qualities
    },

    #' @field features (`data.table()`)\cr
    #' Information about data set features (including target), downloaded from the JSON API response and
    #'   converted to a [data.table()`] with columns:
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
        private$.features = cached(download_data_features, "data_features", self$id, use_cache = self$use_cache)
      }
      private$.features
    },

    #' @field data (`data.table()`)\cr
    #' Data as `data.table()`.
    #' Columns marked as row identifiers or marked with the ignore flag are automatically removed.
    data = function() {
      if (is.null(private$.data)) {
        private$.data = cached(download_data, "data", self$id, description = self$description, use_cache = self$use_cache)
      }

      private$.data
    },

    #' @field target_names (`character()`)\cr
    #' Name of the default target, as extracted from the OpenML data set description.
    target_names = function() {
      self$description$default_target_attribute
    },

    #' @field feature_names (`character()`)\cr
    #' Name of the features, as extracted from the OpenML data set description.
    feature_names = function() {
      self$features[!is_target & !is_ignore & !is_row_identifier, name]
    },

    #' @field nrow (`integer()`)\cr
    #' Number of observations, as extracted from the OpenML data set qualities.
    nrow = function() {
      as.integer(self$quality("NumberOfInstances"))
    },

    #' @field ncol (`integer()`)\cr
    #' Number of features (including targets), as extracted from the OpenML data set qualities.
    ncol = function() {
      as.integer(self$quality("NumberOfFeatures"))
    },

    #' @field task ([mlr3::Task])\cr
    #' Creates a [mlr3::Task] using the default target attribute stored in the data set.
    task = function() {
      target = self$target_names
      switch(as.character(self$features[.(target), data_type, on = "name"]),
        "nominal" = TaskClassif$new(self$name, self$data, target = target),
        "numeric" = TaskRegr$new(self$name, self$data, target = target),
        stop("Unknown task type")
      )
    }
  ),

  private = list(
    .data = NULL,
    .description = NULL,
    .qualities = NULL,
    .features = NULL
  )
)
