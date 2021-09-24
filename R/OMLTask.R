#' @title Interface to OpenML Tasks
#'
#' @description
#' This is the class for tasks served on \url{https://openml.org/t}.
#'
#' @section mlr3 Integration:
#' A [mlr3::Task] is returned by the method `$task`.
#' Alternatively, you can convert this object to a [mlr3::DataBackend] using
#' `mlr3::as_data_backend()`.
#'
#' @references
#' `r format_bib("vanschoren2014")`
#'
#' @export
#' @examples
#' \donttest{
#' otask = OMLTask$new(id = 59)
#'
#' print(otask)
#' print(otask$target_names)
#' print(otask$feature_names)
#' print(otask$tags)
#' print(otask$task)
#'
#' # get a task via tsk():
#' if (requireNamespace("mlr3")) {
#'   mlr3::tsk("oml", task_id = 59)
#' }
#' }
OMLTask = R6Class("OMLTask",
  public = list(
    #' @field id (`integer(1)`)\cr
    #' OpenML task id.
    id = NULL,

    #' @template field_cache_dir
    cache_dir = NULL,

    #' @description
    #' Creates a new object of class `OMLTask`.
    #'
    #' @param id (`integer(1)`)\cr
    #'   OpenML task id.
    #' @template param_cache
    initialize = function(id, cache = getOption("mlr3oml.cache", FALSE)) {
      self$id = assert_count(id, coerce = TRUE)
      self$cache_dir = get_cache_dir(cache)
      initialize_cache(self$cache_dir)
    },

    #' @description
    #' Prints the object.
    #' For a more detailed printer, convert to a [mlr3::Task] via `$task`.
    print = function() {
      catf("<OMLTask:%i:%s> (%ix%i)", self$id, self$name, self$nrow, self$ncol)
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
        private$.desc = cached(download_task_desc, "task_desc", self$id, cache_dir = self$cache_dir)
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
        private$.data = OMLData$new(self$data_id, cache = self$cache_dir)
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
      source_data = self$desc$input$source_data
      targets = switch(self$desc$task_type,
        "Supervised Classification" =,
        "Supervised Regression" = source_data$target_feature,
        "Survival Analysis" = unlist(source_data[c("target_feature_left", "target_feature_right", "target_feature_event")], use.names = FALSE),
        stopf("Unsupoorted task type '%s'", self$desc$task_type)
      )

      make.names(targets)
    },

    #' @field feature_names (`character()`)\cr
    #' Name of the features (without targets of this [OMLTask]).
    feature_names = function() {
      setdiff(c(self$data$target_names, self$data$feature_names), self$target_names)
    },

    #' @field task ([mlr3::Task])\cr
    #' Creates a [mlr3::Task] using the target attribute of the task description.
    task = function() {
      name = self$name
      data = self$data$data
      target = self$target_names

      miss = setdiff(target, names(data))
      if (length(miss)) {
        stopf("Task %i could not be created: target '%s' not found in data", self$id, miss[1L])
      }

      constructor = switch(self$desc$task_type,
        # FIXME: positive class?
        "Supervised Classification" = new_task_classif,
        "Supervised Regression" = new_task_regr,
        "Survival Analysis" = new_task_surv,
        stopf("Unsupoorted task type '%s'", self$desc$task_type)
      )
      task = constructor(name, data, target = target)
      task$backend$hash = sprintf("mlr3oml::task_%i", self$id)
      task
    },

    #' @field resampling ([mlr3::Resampling])\cr
    #' Creates a [ResamplingCustom][mlr3::mlr_resamplings_custom] using the target attribute of the task description.
    resampling = function() {
      if (is.null(private$.resampling)) {
        type = NULL
        splits = cached(download_task_splits, "task_splits", self$id, self$desc, cache_dir = self$cache_dir)
        train_sets = splits[type == "TRAIN", list(row_id = list(as.integer(rowid) + 1L)),
          keyby = c("repeat.", "fold")]$row_id
        test_sets = splits[type == "TEST", list(row_id = list(as.integer(rowid) + 1L)),
          keyby = c("repeat.", "fold")]$row_id

        resampling = mlr3::ResamplingCustom$new()
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

#' @importFrom mlr3 as_task
#' @export
as_task.OMLTask = function(x, ...) {
  x$task
}
