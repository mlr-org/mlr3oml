#' @title Interface to OpenML Tasks
#'
#' @description
#' This is the class for tasks served on \url{https://openml.org/t}.
#' @export
OMLTask = R6Class("OMLTask",
  public = list(
    #' @field id (`integer(1)`)\cr
    #' OpenML task id.
    id = NULL,

    #' @field use_cache (`logical(1)`)\cr
    #' If `TRUE`, locally caches downloaded objects on the file system.file.
    #' See [R_user_dir()] for the path (depending on your operating system).
    use_cache = NULL,

    #' @description
    #' Creates a new object of class `OMLTask`.
    #'
    #' @param id (`integer(1)`)\cr
    #'   OpenML task id.
    #' @param use_cache (`logical(1)`)\cr
    #'   Flag to control the file system cache.
    #'   See [R_user_dir()] for the path (depending on your operating system).
    #'   Default is the value of option `"mlr3oml.use_cache"` or `FALSE`.
    initialize = function(id, use_cache = getOption("mlr3oml.use_cache", FALSE)) {
      self$id = assert_count(id, coerce = TRUE)
      self$use_cache = assert_flag(use_cache)
    }
  ),

  active = list(
    #' @field name (`character(1)`)\cr
    #'   Name of the task, as extracted from the task description.
    name = function() {
      self$desc$task_name
    },

    #' @field desc (`list()`)\cr
    #'   Task description (meta information), downloaded and converted from the JSON API response.
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = cached(download_task_desc, "task_desc", self$id, use_cache = self$use_cache)
     }

      private$.desc
    },

    #' @field data_id (`integer()`)\cr
    #'   Data id, extracted from the task description.
    data_id = function() {
      self$desc$input$source_data$data_set_id
    },

    #' @field data ([OMLData])\cr
    #' Access to the underlying OpenML data set via a [OMLData] object.
    data = function() {
      if (is.null(private$.data)) {
        private$.data = OMLData$new(self$data_id, use_cache = self$use_cache)
      }

      private$.data
    },

    #' @field nrow (`integer()`)\cr
    #' Number of rows, as extracted from the [OMLData] object.
    nrow = function() {
      self$data$nrow
    },

    #' @field ncol (`integer()`)\cr
    #' Number of columns, as extracted from the [OMLData] object.
    ncol = function() {
      self$data$ncol
    },

    #' @field target_names (`character()`)\cr
    #' Name of the targets, as extracted from the OpenML task description.
    target_names = function() {
      make.names(self$desc$input$source_data$target_feature)
    },

    #' @field feature_names (`character()`)\cr
    #' Name of the features, as extracted from the [OMLData] object.
    feature_names = function() {
      self$data$feature_names
    },

    #' @field task ([mlr3::Task])\cr
    #' Creates a [mlr3::Task] using the target attribute of the task desc.
    task = function() {
      task = switch(self$desc$task_type,
        # FIXME: positive class?
        "Supervised Classification" = TaskClassif$new(self$name, self$data$data, target = self$target_names),
        "Supervised Regression" = TaskRegr$new(self$name, self$data$data, target = self$target_names)
      )
      task$backend$hash = sprintf("mlr3oml::task_%i", self$id)
      task
    },

    #' @field resampling ([mlr3::Resampling])\cr
    #' Creates a [mlr3::ResamplingCustim] using the target attribute of the task description.
    resampling = function() {
      if (is.null(private$.resampling)) {
        splits = cached(download_data_splits, "data_splits", self$id, self$desc, use_cache = self$use_cache)
        train_sets = splits[type == "TRAIN", list(row_id = list(as.integer(rowid) + 1L)), keyby = c("repeat.", "fold")]$row_id
        test_sets = splits[type == "TEST", list(row_id = list(as.integer(rowid) + 1L)), keyby = c("repeat.", "fold")]$row_id

        resampling = ResamplingCustom$new()
        private$.resampling = resampling$instantiate(self$task, train_sets = train_sets, test_sets = test_sets)
      }

      private$.resampling
    },

    #' @field tags (`character()`)\cr
    #' Returns all tags of the task.
    tags = function() {
      self$desc$tag
    }
  ),

  private = list(
    .data = NULL,
    .desc = NULL,
    .resampling = NULL
  )
)
