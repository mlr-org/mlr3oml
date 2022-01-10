#' @title OpenML Collection
#'
#' @description
#' This is the class for collections served on \url{https://new.openml.org/search?type=study&study_type=task&sort=tasks_included}. A collection can contain datasets, flows, runs and tasks.
#' These are stored as an OMLContainer.
#'
#' @section mlr3 Intergration:
#' The tasks can be converted to a list of [mlr3::Task]s by calling `$convert()` on the field
#' `tasks`.
#' The flows can be converted to a list of [mlr3::Learner]s by calling `$convert()` on the field
#' `learners`.
#' The runs can be converted to a list of [mlr3::ResampleResult]s by calling `$convert` on the
#' field `runs`.
#'
#'
#' @references
#' `r format_bib("vanschoren2014")`
#' @export
#'
OMLCollection = R6::R6Class("OMLCollection",
  public = list(
    #' @field id (`integer(1)`)\cr
    #' OpenML collection id.
    id = NULL,

    #' @template field_cache_dir
    cache_dir = NULL,

    #' @field (`logical(1))\cr
    #' Whether the downloaded data is cached.
    cache = NULL,

    #' @description
    #' Creates a new object of class `OMLCollection`.
    #'
    #' @param id (`integer(1)`)\cr
    #'  OpenML run id.
    #' @template param_cache
    initialize = function(id, cache = getOption("mlr3oml.cache", FALSE)) {
      self$id = assert_count(id, coerce = TRUE)
      self$cache_dir = get_cache_dir(assert_flag(cache))
      self$cache = cache
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
    convert = function() {

    }
  ),
  active = list(
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = cached(download_collection_desc,
          type = "collection", id = self$id,
          cache_dir = self$cache_dir
        )
      }
      return(private$.desc)
    },

    #' @field (`character(1)`) name\cr
    #' The name of the study.
    name = function() self$desc$name,

    #' @field (`integer(n)`)\cr
    #' The study creators.
    creator = function() self$desc$creator,

    #' @field datetime (`.POSIXct(1)`)\cr
    #' The datetime that the dataset was uploaded.
    creation_date = function() self$desc$creation_date,

    #' @field alias (`character(n)`)\cr
    #' The alias of the study.
    alias = function() self$desc$alias,

    #' @field tag (`character(n)`)\cr
    #' A character vector containing the tags.
    tag = function() self$desc$tag,

    #' @field main_entity_type (`character(1)`)\cr
    #' The main entity type of the collection.
    main_entity_type = function() self$desc$main_entity_type,

    #' @field description (`character(1)`) \cr
    #' Description of the collection.
    description = function() self$desc$description,

    #' @field flow_ids (`integer(n)`)\cr
    #' An vector containing the flow ids of the collection.
    flow_ids = function() self$desc$flow$flow_id,

    #' @field data_ids (`integer(n)`)\cr
    #' An vector containing the data ids of the collection.
    data_ids = function() self$desc$data$data_id,

    #' @field run_ids (`integer(n)`)\cr
    #' An vector containing the run ids of the collection.
    run_ids = function() self$desc$runs$run_id,

    #' @field task_ids (`integer(n)`)\cr
    #' An vector containing the task ids of the collection.
    task_ids = function() self$desc$task$task_id,

    #' @field resampling_ids (`integer(n)`)\cr
    #' An vector containing the resampling ids of the collection.
    #' Note that this corresponds exactly to the task ids.
    resampling_ids = function() self$desc$task$task_id,

    #' @field runs (`list(n)')
    #' A list of [mlr3oml][mlr3oml::OMLRun]s.
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
    #' A list of [mlr3oml][mlr3oml::OMLFlow]s.
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
    #' A list of [mlr3oml][mlr3oml::OMLData].
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
    #' A list of [mlr3oml][mlr3oml::OMLTasks]s.
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
