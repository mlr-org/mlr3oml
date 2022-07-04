#' @title OpenML Collection
#'
#' @description
#' This is the class for collections (previously known as studies) served on
#' \url{https://openml.org/search?type=study&study_type=task&sort=tasks_included}.
#' It is used both for Run Collections and Task Collections.
#' (Note that all Benchmark Suites on OpenML are also Collections).
#' A Run Collection (`main_entity_type = "run"`) contains runs, flows, datasets and tasks.
#' A Task Collection (`main_entity_type = "task"`) contains tasks and datasets.
#'
#' @section mlr3 Intergration:
#'  * Obtain a list of [mlr3::Task]s using `as_tasks()`.
#'  * Obtain a list of [mlr3::Resampling]s using `as_resamplings()`.
#'  * Obtain a list of [mlr3::Learner]s using `as_learners()` (if main_entity_type is "run").
#'  * Obtain a [mlr3::BenchmarkResult] using `as_benchmark_result()` (if main_entity_type is "run").
#'
#' @references
#' `r format_bib("vanschoren2014")`
#' @export
#' @examples
#' \donttest{
#' library("mlr3")
#' # OpenML Run Collection:
#' collection = OMLCollection$new(232L)
#' collection$tasks
#' collection$data
#' collection$flows
#' collection$runs
#'
#' # mlr3 conversion:
#' tasks = as_tasks(collection)
#' resamplings = as_resamplings(collection)
#' # construct pseudo-learners as these are sklearn flows
#' learners = as_learners(collection, "classif")
#'
#' # Although pseudo-learners are non-executable the runs can still be analyzed.
#' bmr = as_benchmark_result(collection)
#' bmr$score(msr("classif.ce"))
#' }
OMLCollection = R6Class("OMLCollection",
  public = list(
    #' @field id (`integer(1)`)\cr
    #'   OpenML collection id.
    id = NULL,
    #' @template field_cache_dir
    cache_dir = NULL,
    #' @description
    #' Creates a new object of class `OMLCollection`.
    #' @param id (`integer(1)`)\cr
    #'  OpenML run id.
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
    print = function() {
      catf("<OMLCollection: %i>", self$id)
      catf(" * data:  %i", length(self$data_ids))
      catf(" * tasks: %i", length(self$task_ids))
      if (self$main_entity_type == "run") {
        catf(" * flows: %i", length(self$flow_ids))
        catf(" * runs:  %i", length(self$run_ids))
      }
    }
  ),
  active = list(
    #' @field desc (`list()`)\cr
    #'   Colllection description (meta information), downloaded and converted from the JSON API response.
    #'   This cannot be cached, because it can be modified on the server.
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = cached(download_collection_desc,
          type = "collection", id = self$id,
          cache_dir = FALSE
        )
      }
      return(private$.desc)
    },
    #' @field name (`character(1)`) \cr
    #'   The name of the collection.
    name = function() self$desc$name,
    #' @field tags (`character(n)`)\cr
    #'   The tags of the OpenML collection.
    tags = function() self$desc$tag,
    #' @field main_entity_type (`character(1)`)\cr
    #'   The main entity type of the collection (either "run" or "task").
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
          function(x) OMLRun$new(x, cache = is.character(self$cache_dir), parquet = self$parquet)
        )

        private$.runs = make_run_table(runs)
      }
      return(private$.runs)
    },
    #' @field flows (`data.table()`)
    #'   A data.table summarizing the flows included in the collection. Returns NULL for
    #'   Task Collections.
    flows = function() {
      if (self$main_entity_type == "task") {
        messagef("Main entity type is task, returning NULL.")
        return(NULL)
      }
      if (is.null(private$.flows)) {
        flows = map(
          self$flow_ids,
          function(x) OMLFlow$new(x, cache = is.character(self$cache_dir))
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
          function(x) OMLData$new(x, cache = is.character(self$cache_dir), parquet = self$parquet)
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
          function(x) OMLTask$new(x, cache = is.character(self$cache_dir), parquet = self$parquet)
        )
        private$.tasks = make_task_table(tasks)
      }
      return(private$.tasks)
    },
    #' @field parquet (`logical(1)`)\cr
    #' Whether to use parquet.
    parquet = function(rhs) {
      assert_ro_binding(rhs)
      private$.parquet
    }
  ),
  private = list(
    .desc = NULL,
    .runs = NULL,
    .tasks = NULL,
    .resamplings = NULL,
    .data = NULL,
    .flows = NULL,
    .parquet = FALSE
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

#'
make_task_table = function(tasks) {
  g = function(task) {
    list(
      id = task$id,
      task = list(task),
      data = task$data$name,
      task_type = task$task_type,
      target = tryCatch(task$target_names, error = function(x) NA_character_), # can have length > 1
      nrow = as.integer(task$data$quality("NumberOfInstances")),
      ncol = task$data$quality("NumberOfFeatures"),
      missing = task$data$quality("NumberOfMissingValues"),
      numeric = task$data$quality("NumberOfNumericFeatures"),
      symbolic = task$data$quality("NumberOfSymbolicFeatures"),
      binary = task$data$quality("NumberOfBinaryFeatures"),
      data_split = task$data_split$type
    )
  }
  setkeyv(map_dtr(tasks, g, .fill = TRUE), "id")[]
}

make_flow_table = function(flows) {
  g = function(flow) {
    list(
      id = flow$id,
      flow = list(flow),
      name = flow$name
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
      data = run$desc$input_data$dataset$name,
      flow = as_short_string(run$desc$flow_name),
      data_split = run$task$data_split$type
    )
  }
  setkeyv(map_dtr(runs, g, .fill = TRUE), "id")[]
}
