#' @title OpenML Collection
#'
#' @description
#' This is the class for collections (previously known as studies) served on
#' \url{https://new.openml.org/search?type=study&study_type=task&sort=tasks_included}.
#' A collection can contain datasets, flows, runs and tasks. These are stored as a [OMLDictionary].
#'
#' @section mlr3 Intergration:
#'  * In caste the `main_entity_type`is `run`, `$convert()`$ returns a [mlr3::BenchmarkResult].
#'  * In caste the `main_entity_type`is `task`, `$convert()`$ returns a list of [mlr3::Task]s and
#'    corresponding [mlr3::Resampling]s.
#'
#' @references
#' `r format_bib("vanschoren2014")`
#' @export
#'
OMLCollection = R6::R6Class("OMLCollection",
  public = list(
    #' @field id (`integer(1)`)\cr
    #'   OpenML collection id.
    id = NULL,

    #' @template field_cache_dir
    cache_dir = NULL,

    #' @description
    #' Creates a new object of class `OMLCollection`.
    #'
    #' @param id (`integer(1)`)\cr
    #'  OpenML run id.
    #' @template param_cache
    initialize = function(id, cache = getOption("mlr3oml.cache", FALSE)) {
      self$id = assert_count(id, coerce = TRUE)
      self$cache_dir = get_cache_dir(assert_flag(cache))
      initialize_cache(self$cache_dir)
    },
    #' @description
    #' Prints the object.
    print = function() {
      catf("<OMLCollection: %i>", self$id)
      catf(" * data:  %i", length(self$data_ids))
      catf(" * flows: %i", length(self$flow_ids))
      catf(" * runs:  %i", length(self$run_ids))
      catf(" * tasks: %i", length(self$task_ids))
    },
    #' @description
    #' Converts the object to either a list of [mlr3::Task]s (main_entity_type = "task") or a
    #' [mlr3::BenchmarkResult] (main_entity_type = "run").
    convert = function() {
      if (self$main_entity_type == "task") {
        tasks = self$tasks$mget(self$task_ids, convert = TRUE)
        resamplings = self$tasks$mget_rsmp(self$task_ids, convert = TRUE)
        output = list(task = tasks, resampling = resamplings)
        return(output)
      }
      rrs = self$runs$mget(self$run_ids, convert = TRUE)
      bmr = mlr3::as_benchmark_result(invoke(c, .args = rrs))
      return(bmr)
    }
  ),
  active = list(

    #' @field desc (`list()`)\cr
    #'   Colllection description (meta information), downloaded and converted from the JSON API response.
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = cached(download_collection_desc,
          type = "collection", id = self$id,
          cache_dir = self$cache_dir
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

    #' @field runs (`list(n)')
    #'   A list of [mlr3oml::OMLRun]s.
    runs = function() {
      if (is.null(private$.runs)) {
        private$.runs = OMLDictionaryRun$new()
        runs = map(
          self$run_ids,
          function(x) OMLRun$new(x, cache = is.character(self$cache_dir))
        )
        map(runs, function(run) private$.runs$add(run))
      }
      return(private$.runs)
    },

    #' @field flows (`list(n)')
    #'   A Dictionary of [mlr3oml::OMLFlow]s.
    flows = function() {
      if (is.null(private$.flows)) {
        private$.flows = OMLDictionaryFlow$new()
        flows = map(
          self$flow_ids,
          function(x) OMLFlow$new(x, cache = is.character(self$cache_dir))
        )
        map(flows, function(flow) private$.flows$add(flow))
      }
      return(private$.flows)
    },

    #' @field data (`list(n)')
    #'   A Dictionary of [mlr3oml::Data] Sets.
    data = function() {
      if (is.null(private$.data)) {
        private$.data = OMLDictionaryData$new()
        data = mlr3misc::map(
          self$data_ids,
          function(x) OMLData$new(x, cache = is.character(self$cache_dir))
        )
        map(data, function(data) private$.data$add(data))
      }
      return(private$.data)
    },

    #' @field tasks (`list(n)')
    #'   A Dictionary of [OMLTasks]s and the corresponding [OMLResampling]s.
    tasks = function() {
      if (is.null(private$.tasks)) {
        tasks = map(
          self$task_ids,
          function(x) OMLTask$new(x, cache = is.character(self$cache_dir))
        )
        private$.tasks = OMLDictionaryTask$new()
        map(tasks, function(task) private$.tasks$add(task))
      }
      return(private$.tasks)
    }
  ),
  private = list(
    .desc = NULL,
    .runs = NULL,
    .tasks = NULL,
    .resamplings = NULL,
    .data = NULL,
    .flows = NULL
  )
)
