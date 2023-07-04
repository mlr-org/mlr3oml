#' @title OpenML Collection
#'
#' @name oml_collection
#'
#' @description
#' This is the class for collections (previously known as studies) served on
#' \url{https://www.openml.org}.
#' A collection can either be a [task collection](https://www.openml.org/search?type=study&study_type=task)
#' or [run collection](https://www.openml.org/search?type=study&study_type=run).
#' This object can also be constructed using the sugar function [ocl()].
#'
#' **Run Collection**
#'
#' A run collection contains runs, flows, datasets and tasks.
#' The primary object are the runs (`main_entity_type` is `"run"`).
#' The the flows, datasets and tasks are those used in the runs.
#'
#' **Task Collection**
#' A task collection (`main_entity_type = "task"`) contains tasks and datasets.
#' The primary object are the tasks (`main_entity_type` is `"task"`).
#' The datasets are those used in the tasks.
#'
#' *Note*: All Benchmark Suites on OpenML are also collections.
#'
#' @section Caching:
#' Because collections on OpenML can be modified (ids can be added), it is not possible to cache
#' this object.
#'
#' @section mlr3 Intergration:
#'  * Obtain a list of [mlr3::Task]s using [mlr3::as_tasks].
#'  * Obtain a list of [mlr3::Resampling]s using [mlr3::as_resamplings].
#'  * Obtain a list of [mlr3::Learner]s using [mlr3::as_learners] (if main_entity_type is "run").
#'  * Obtain a [mlr3::BenchmarkResult] using [mlr3::as_benchmark_result] (if main_entity_type is "run").
#'
#' @references
#' `r format_bib("vanschoren2014")`
#' @export
#' @template examples
OMLCollection = R6Class("OMLCollection",
  inherit = OMLObject,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_id
    #' @template param_test_server
    initialize = function(
      id,
      test_server = test_server_default()
      ) {
      super$initialize(id, test_server, "collection")
    },
    #' @description
    #' Prints the object.
    print = function() {
      # trigger download first for better printing
      self$desc
      catf("<OMLCollection: %i> %s", self$id, as_short_string(self$name))
      catf(" * data:  %i", length(self$data_ids))
      catf(" * tasks: %i", length(self$task_ids))
      if (self$main_entity_type == "run") {
        catf(" * flows: %i", length(self$flow_ids))
        catf(" * runs:  %i", length(self$run_ids))
      }
      if (self$test_server) {
        catf(" * Using test server")
      }
    },
    #' @description
    #' Downloads the whole object for offline usage.
    download = function() {
      self$desc
      invisible(self)
    }
  ),
  active = list(
     #' @field desc (`list()`)\cr
    #'   Colllection description (meta information), downloaded and converted from the JSON API response.
    desc = function() {
      if (is.null(private$.desc)) {
        # note that we DONT CACHE HERE
        private$.desc = cached(download_desc_collection,
          type = "collection", id = self$id, cache_dir = FALSE, server = self$server,
          test_server = self$test_server
        )
      }
      return(private$.desc)
    },
    #' @field parquet (`logical(1)`)\cr
    #' Whether to use parquet.
    parquet = function(rhs) {
      assert_ro_binding(rhs)
      private$.parquet
    },
    #' @field main_entity_type (`character(n)`)\cr
    #'   The main entity type, either `"run"` or `"task"`.
    main_entity_type = function() self$desc$main_entity_type,
    #' @field flow_ids (`integer(n)`)\cr
    #'   An vector containing the flow ids of the collection.
    flow_ids = function() self$desc$flow$flow_id,
    #' @field data_ids (`integer(n)`)\cr
    #'   An vector containing the data ids of the collection.
    data_ids = function() self$desc$data$data_id,
    #' @field run_ids (`integer(n)`)\cr
    #'   An vector containing the run ids of the collection.
    run_ids = function() self$desc$runs$run_id,
    #' @field task_ids (`integer(n)`)\cr
    #'   An vector containing the task ids of the collection.
    task_ids = function() self$desc$task$task_id
  ),
  private = list(
    .runs = NULL,
    .tasks = NULL,
    .resamplings = NULL,
    .data = NULL,
    .flows = NULL,
    .parquet = NULL
  )
)

#' @importFrom mlr3 as_benchmark_result
#' @export
as_benchmark_result.OMLCollection = function(x, ...) {
  assert_true(x$main_entity_type == "run")
  rrs = map(x$run_ids, function(id) as_resample_result(OMLRun$new(id, ...)))
  bmr = as_benchmark_result(invoke(c, .args = rrs))
  return(bmr)
}

#' @importFrom mlr3 as_tasks
#' @export
as_tasks.OMLCollection = function(x, ...) {
  map(x$task_ids, function(id) tsk("oml", task_id = id, ...))
}

#' @importFrom mlr3 as_resamplings
#' @export
as_resamplings.OMLCollection = function(x, ...) {
  map(x$task_ids, function(id) rsmp("oml", task_id = id, ...))
}

#' @importFrom mlr3 as_learners
#' @export
as_learners.OMLCollection = function(x, ...) {
  map(x$flow_ids, function(id) as_learner(OMLFlow$new(id, ...)))
}
