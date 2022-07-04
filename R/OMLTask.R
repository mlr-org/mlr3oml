#' @title Interface to OpenML Tasks
#'
#' @description
#' This is the class for tasks served on [OpenML](https://openml.org/search?type=task&sort=runs).
#' It consists of a dataset and other meta-information such as the target variable for supervised
#' problems.
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
#' library("mlr3")
#' \donttest{
#' # Get a task from OpenML:
#' otask = OMLTask$new(id = 31L)
#' otask$data
#' otask$data_split
#' otask$target_names
#' otask$feature_names
#'
#' # convert to mlr3 Task:
#' task = as_task(otask)
#'
#' # get a task via tsk():
#' tsk("oml", task_id = 31L)
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
    #' @param id (`integer(1)`)\cr
    #'   OpenML task id.
    #' @template param_cache
    #' @template param_parquet
    initialize = function(id, cache = getOption("mlr3oml.cache", FALSE),
      parquet = getOption("mlr3oml.parquet", FALSE)) {
      self$id = assert_count(id, coerce = TRUE)
      self$cache_dir = FALSE
      private$.parquet = parquet
      initialize_cache(self$cache_dir)
    },
    #' @description
    #' Prints the object.
    #' For a more detailed printer, convert to a [mlr3::Task] via `$task`.
    print = function() {
      catf("<OMLTask:%i>", self$id)
      catf(" * Type: %s", self$desc$task_type)
      catf(" * Data: %s (%ix%i)", self$data_name, self$nrow, self$ncol)
      if (self$task_type %in% c("Supervised Regression", "Supervised Classification")) {
        catf(" * Target: %s", paste(self$target_names, collapse = ","))
      }
      catf(" * Data split: %s", self$data_split$type)
    }
  ),
  active = list(
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
    #' @field desc (`list()`)\cr
    #'   Task description (meta information), downloaded and converted from the JSON API response.
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = cached(download_task_desc, "task_desc", self$id, cache_dir = FALSE)
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
        private$.data = OMLData$new(self$data_id, cache = is.character(self$cache_dir),
          parquet = self$parquet
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
    #' @field data_split ([mlr3oml::OMLDataSplit])\cr
    #' Access to the OpenML Evaluation Procedure.
    data_split = function() {
      if (!length(self$desc$inpu$estimation_procedure$id)) { # integer(0)
        warningf("Task %s does not provide data split, returning NULL.", self$id)
        return(NULL)
      }
      if (is.null(private$.data_split)) {
        private$.data_split = OMLDataSplit$new(task = self, cache = is.character(self$cache_dir))
      }
      private$.data_split
    },
    #' @field tags (`character()`)\cr
    #' Returns all tags of the task.
    tags = function() {
      self$desc$tag
    },
    #' @field data_name (`character()`)\cr
    #' Name of the dataset (inferred from the task name).
    data_name = function() {
      strsplit(self$desc$task_name, split = " ")[[1]][[3]]
    },
    #' @field parquet (`logical(1)`)\cr
    #' Whether to use parquet.
    parquet = function(rhs) {
      assert_ro_binding(rhs)
      private$.parquet
    }
  ),
  private = list(
    .data = NULL,
    .desc = NULL,
    .data_split = NULL,
    .parquet = NULL
  )
)

#' @importFrom mlr3 as_task
#' @export
as_task.OMLTask = function(x, ...) {
  name = x$data$name
  data = x$data$data
  data = remove_named(data, c(x$desc$row_id_attribute, x$desc$ignore_attribute))
  target = x$target_names

  miss = setdiff(target, names(data))
  if (length(miss)) {
    stopf("Task %i could not be created: target '%s' not found in data", x$id, miss[1L])
  }

  constructor = switch(x$desc$task_type,
    # FIXME: positive class?
    "Supervised Classification" = new_task_classif,
    "Supervised Regression" = new_task_regr,
    # "Survival Analysis" = new_task_surv,
    stopf("Unsupported task type '%s'.", x$desc$task_type)
  )
  task = constructor(name, data, target = target)
  task$backend$hash = sprintf("mlr3oml::task_%i", x$id)
  task$.__enclos_env__$private$oml$id = x$id
  task$.__enclos_env__$private$oml$hash = task$hash
  return(task)
}

#' @export
as_resampling.OMLTask = function(x, ...) {
  data_split = x$data_split
  if (is.null(data_split)) {
    return(NULL)
  }
  as_resampling(x$data_split, ...)
}
