#' @title OpenML Collection
#'
#' @name oml_collection_r6
#'
#' @description
#' This is the class for collections (previously known as studies) served on
#' \url{https://www.openml.org}.
#' A collection can either be a [task collection](https://www.openml.org/search?type=study&study_type=task)
#' or [run collection](https://www.openml.org/search?type=study&study_type=run).
#' This object can also be constructed using the sugar function [oml_collection()].
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
#' The OpenML collection itself cannot be not cached, this is because it can be modified in-place
#' on the server, e.g. by adding or removing tasks or runs.
#' The construction argument `cache` therefore only controls wether caching is applied to the
#' OpenML objects that are contained in the collection.
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
#' @examples
#' \donttest{
#' library("mlr3")
#' # OpenML Run collection:
#' run_collection = OMLCollection$new(id = 232)
#' # using sugar
#' run_collection = oml_collection(id = 232)
#' run_collection$main_entity_type
#' run_collection$tasks
#' run_collection$data
#' run_collection$flows
#' run_collection$runs
#'
#' # mlr3 conversion:
#' tasks = as_tasks(run_collection)
#' resamplings = as_resamplings(run_collection)
#' learners = as_learners(run_collection, "classif")
#'
#' bmr = as_benchmark_result(run_collection)
#' bmr$score(msr("classif.ce"))
#'
#' # OpenML task collection
#' task_collection = OMLCollection$new(id = 258)
#' # using sugar
#' task_collection = oml_collection(id = 258)
#'
#' task_collection$main_entity_type
#' task_collection$tasks
#' task_collection$data
#'
#' # mlr3 conversion
#' tasks = as_tasks(task_collection)
#' resamplings = as_resamplings(task_collection)
#' }
OMLCollection = R6Class("OMLCollection",
  inherit = OMLObject,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_id
    #' @param cache (`logical(1)` | `character(1)`)\cr
    #' See field `cache` for an explanation of possible values.
    #' Defaults to value of option `"mlr3oml.cache"`, or `FALSE` if not set.
    #' The collection itself is not cached, this is because it can be modified in-place on OpenML,
    #' e.g. by adding or removing tasks or runs. This parameter therefore only controls whether
    #' the contained elements are cached when loaded, e.g. when accessing the included tasks.
    #' @template param_parquet
    #' @template param_test_server
    initialize = function(
      id,
      cache = cache_default(),
      parquet = parquet_default(),
      test_server = test_server_default()
      ) {
      private$.parquet = assert_flag(parquet)
      super$initialize(id, cache, test_server, "collection")
    },
    #' @description
    #' Prints the object.
    print = function() {
      catf("<OMLCollection: %i>", self$id)
      catf(" * data:  %i", length(self$data_ids))
      catf(" * tasks: %i", length(self$task_ids))
      if (self$main_entity_type == "run") {
        catf(" * flows: %i", length(self$flow_ids))
        catf(" * runs:  %i", length(self$run_ids))
      }
      if (self$test_server) {
        catf(" * Using test server")
      }
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
    task_ids = function() self$desc$task$task_id,
    #' @field runs (`data.table()`)
    #'   A data.table summarizing the runs included in the collection. Returns NULL for
    #'   Task Collections.
    runs = function() {
      if (self$main_entity_type == "task") {
        messagef("Main entity type is task, returning NULL.")
        return(NULL)
      }
      if (is.null(private$.runs)) {
        runs = map(
          self$run_ids,
          function(x) OMLRun$new(x, cache = self$cache_dir, parquet = self$parquet,
            test_server = self$test_server
          )
        )

        private$.runs = make_run_table(runs)
      }
      return(private$.runs)
    },
    #' @field flows (`data.table()`)
    #'   A data.table summarizing the flows included in the collection. Returns `NULL` for
    #'   Task Collections.
    flows = function() {
      if (self$main_entity_type == "task") {
        messagef("Main entity type is task, returning NULL.")
        return(NULL)
      }
      if (is.null(private$.flows)) {
        flows = map(
          self$flow_ids,
          function(x) OMLFlow$new(x, cache = self$cache_dir, test_server = self$test_server)
        )
        private$.flows = make_flow_table(flows)
      }
      return(private$.flows)
    },
    #' @field data (`data.table()`)
    #'   A data.table summarizing the datasets included in the collection.
    data = function() {
      if (is.null(private$.data)) {
        datasets = map(
          self$data_ids,
          function(x) OMLData$new(x, cache = self$cache_dir, parquet = self$parquet,
            test_server = self$test_server
          )
        )
        private$.data = make_dataset_table(datasets)
      }
      return(private$.data)
    },
    #' @field tasks (`data.table()`)
    #'   A data.table summarizing the tasks included in the collection.
    tasks = function() {
      if (is.null(private$.tasks)) {
        tasks = map(
          self$task_ids,
          function(x) OMLTask$new(x, cache = self$cache_dir, parquet = self$parquet,
            test_server = self$test_server
          )
        )
        private$.tasks = make_task_table(tasks)
      }
      return(private$.tasks)
    }
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
  rrs = map(x$runs[["run"]], as_resample_result)
  bmr = as_benchmark_result(invoke(c, .args = rrs))
  return(bmr)
}

#' @importFrom mlr3 as_tasks
#' @export
as_tasks.OMLCollection = function(x, ...) {
  map(x$tasks[["task"]], as_task, ...)
}

#' @importFrom mlr3 as_learners
#' @export
as_learners.OMLCollection = function(x, ...) {
  map(x$flows[["flow"]], as_learner, ...)
}

#' @importFrom mlr3 as_resamplings
#' @export
as_resamplings.OMLCollection = function(x, ...) {
  map(x$tasks[["task"]], as_resampling, ...)
}

make_task_table = function(tasks) {
  g = function(task) {
    list(
      id = task$id,
      task = list(task),
      data = as_short_string(task$data$name),
      task_type = task$task_type,
      target = tryCatch(task$target_names, error = function(x) NA_character_), # can have length > 1
      nrow = as.integer(task$data$quality("NumberOfInstances")),
      ncol = task$data$quality("NumberOfFeatures"),
      missing = task$data$quality("NumberOfMissingValues"),
      numeric = task$data$quality("NumberOfNumericFeatures"),
      symbolic = task$data$quality("NumberOfSymbolicFeatures"),
      binary = task$data$quality("NumberOfBinaryFeatures"),
      task_splits = task$estimation_procedure$type %??% "none"
    )
  }
  setkeyv(map_dtr(tasks, g, .fill = TRUE), "id")[]
}

make_flow_table = function(flows) {
  g = function(flow) {
    list(
      id = flow$id,
      flow = list(flow),
      name = as_short_string(flow$name)
    )
  }
  setkeyv(map_dtr(flows, g), "id")[]
}

make_dataset_table = function(datasets) {
  g = function(dataset) {
    list(
      id = dataset$id,
      data = list(dataset),
      name = dataset$name,
      nrow = as.integer(dataset$quality("NumberOfInstances")),
      ncol = dataset$quality("NumberOfFeatures"),
      missing = dataset$quality("NumberOfMissingValues"),
      numeric = dataset$quality("NumberOfNumericFeatures"),
      symbolic = dataset$quality("NumberOfSymbolicFeatures"),
      binary = dataset$quality("NumberOfBinaryFeatures")
    )
  }
  setkeyv(map_dtr(datasets, g, .fill = TRUE), "id")[]
}

make_run_table = function(runs) {
  g = function(run) {
    list(
      id = run$id,
      run = list(run),
      task_type = run$task_type,
      data = as_short_string(run$desc$input_data$dataset$name),
      flow = as_short_string(run$desc$flow_name),
      task_splits = run$task$estimation_procedure$type
    )
  }
  setkeyv(map_dtr(runs, g, .fill = TRUE), "id")[]
}
