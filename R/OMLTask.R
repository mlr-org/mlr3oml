#' @title Interface to OpenML Tasks
#'
#' @name oml_task
#'
#' @description
#' This is the class for tasks served on [OpenML](https://www.openml.org/search?type=task&sort=runs).
#' It consists of a dataset and other meta-information such as the target variable for supervised
#' problems.
#' This object can also be constructed using the sugar function [otsk()].
#'
#' @section mlr3 Integration:
#' * Obtain a [mlr3::Task] by calling `as_task()`.
#' * Obtain a [mlr3::Resampling] by calling `as_resampling()`.
#'
#' @references
#' `r format_bib("vanschoren2014")`
#'
#' @export
#' @examples
#' try({
#'   library("mlr3")
#'   # Get a task from OpenML:
#'   otask = OMLTask$new(id = 31)
#'   # using sugar
#'   otask = otsk(id = 31)
#'   otask$data
#'   otask$target_names
#'   otask$feature_names
#'
#'   # convert to mlr3 Task:
#'   task = as_task(otask)
#'
#'   # get a task via tsk():
#'   tsk("oml", task_id = 31L)
#'   }, silent = TRUE)
OMLTask = R6Class("OMLTask",
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
      super$initialize(id, cache, test_server, "task")
    },
    #' @description
    #' Prints the object.
    #' For a more detailed printer, convert to a [mlr3::Task] via `$task`.
    print = function() {
      catf("<OMLTask:%i>", self$id)
      catf(" * Type: %s", self$desc$task_type)
      catf(" * Data: %s (id: %s; dim: %ix%i)", self$data_name, self$data_id, self$nrow, self$ncol)
      if (self$task_type %in% c("Supervised Regression", "Supervised Classification")) {
        catf(" * Target: %s", as_short_string(self$target_names))
      }
      catf_estimation_procedure(self$estimation_procedure)
      if (self$test_server) {
        catf(" * Using test server")
      }
    }
  ),
  active = list(
    #' @field estimation_procedure (`list()`)\cr
    #'   The estimation procedure, returns `NULL` if none is available.
    estimation_procedure = function() {
      ep = self$desc$input$estimation_procedure
      if (identical(ep$type, list())) {
        return(NULL)
      }
      ep
    },
    #' @field task_splits (`data.table()`)\cr
    #' A data.table containing the splits as provided by OpenML.
    task_splits = function() {
      if (is.null(self$estimation_procedure)) {
        return(NULL)
      }
      if (is.null(private$.task_splits)) {
        private$.task_splits = cached(download_task_splits,
          "task_splits", id = self$id, desc = self$desc, cache_dir = self$cache_dir,
          test_server = self$test_server, server = self$server
        )
      }
      return(private$.task_splits)
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
    #' @field name (`character(1)`)\cr
    #'   Name of the task, extracted from the task description.
    name = function() {
      self$desc$task_name
    },
    #' @field task_type (`character(1)`)\cr
    #'   The OpenML task type.
    task_type = function() {
      self$desc$task_type
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
        private$.data = OMLData$new(self$data_id, cache = self$cache_dir,
          parquet = self$parquet, test_server = self$test_server
        )
      }

      private$.data
    },
    #' @field nrow (`integer()`)\cr
    #' Number of rows, extracted from the [OMLData] object.
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
        "Supervised Classification" = ,
        "Supervised Regression" = source_data$target_feature,
        # "Survival Analysis" = unlist(
        #   source_data[c("target_feature_left", "target_feature_right", "target_feature_event")],
        #   use.names = FALSE
        # ),
        stopf("Unsupported task type '%s'", self$desc$task_type)
      )
      make.names(targets)
    },
    #' @field feature_names (`character()`)\cr
    #' Name of the features (without targets of this [OMLTask]).
    feature_names = function() {
      setdiff(c(self$data$target_names, self$data$feature_names), self$target_names)
    },
    #' @field data_name (`character()`)\cr
    #' Name of the dataset (inferred from the task name).
    data_name = function() {
      strsplit(self$desc$task_name, split = " ")[[1]][[3]]
    }
  ),
  private = list(
    .data = NULL,
    .task_splits = NULL,
    .parquet = NULL
  )
)

#' @importFrom mlr3 as_task
#' @export
as_task.OMLTask = function(x, ...) {
  name = x$data$name

  target = x$target_names
  feature_names = x$feature_names
  backend = as_data_backend(x)
  miss_target = setdiff(target, backend$colnames)
  if (length(miss_target)) {
    stopf("Task %i could not be created: target '%s' not found in data", x$id, miss_target[1L])
  }
  miss_features = setdiff(feature_names, backend$colnames)
  if (length(miss_features)) {
    stopf("Task %i could not be created: features %s not found in data", x$id,
      paste0("'", miss_features, "'", collapse = ", "))
  }

  constructor = switch(x$desc$task_type,
    # FIXME: positive class?
    "Supervised Classification" = TaskClassif,
    "Supervised Regression" = TaskRegr,
    # "Survival Analysis" = new_task_surv,
    stopf("Unsupported task type '%s'.", x$desc$task_type)
  )
  task = constructor$new(name, backend, target = target)
  task$col_roles$feature = feature_names
  return(task)
}

#' @export
as_resampling.OMLTask = function(x, task = NULL, ...) {
  task_splits = x$task_splits
  if (is.null(task_splits)) {
    stopf("OpenML task with id %s does not have task splits.", x$id)
  }
  train_sets = task_splits[get("type") == "TRAIN", list(row_id = list(as.integer(rowid) + 1L)),
    keyby = c("repeat.", "fold")]$row_id
  test_sets = task_splits[get("type") == "TEST", list(row_id = list(as.integer(rowid) + 1L)),
    keyby = c("repeat.", "fold")]$row_id

  task = task %??% as_task(x)

  resampling = mlr3::ResamplingCustom$new()
  resampling$instantiate(task, train_sets = train_sets, test_sets = test_sets)
  resampling
}

#' @export
as_data_backend.OMLTask = function(data, primary_key = NULL, ...) {
  as_data_backend(data$data, primary_key = primary_key, ...)
}
