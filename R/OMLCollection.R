#' @title OML Collection
#'
#' @description
#' This is the class for collections served on \url{https://new.openml.org/search?type=study&study_type=task&sort=tasks_included}.
#'
#' @references
#' `r format_bib("vanschoren2014")`
#' @export
OMLCollection = R6::R6Class("OMLCollection",
  public = list(
    #' @field id (`integer(1)`)\cr
    #' OpenML collection id.
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
      catf("<OMLCollection:%i>", self$id)
      catf("  * data:  %i", length(self$data_ids))
      catf("  * flows: %i", length(self$flow_ids))
      catf("  * runs:  %i", length(self$run_ids))
      catf("  * tasks: %i", length(self$task_ids))
    }
  ),

  active = list(
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = cached(download_collection_desc, type = "collection", id = self$id,
                               cache_dir = self$cache_dir)
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

    #' @field runs (`list(n)')
    #' A list of [mlr3oml][mlr3oml::OMLRun]s.
    runs = function() {
      if (is.null(private$.runs)) {
        private$.runs = OMLContainer$new(
          mlr3misc::map(self$run_ids, function (x) OMLRun$new(x))
        )
      }
      return(private$.runs)
    },

    #' @field flows (`list(n)')
    #' A list of [mlr3oml][mlr3oml::OMLFlow]s.
    flows = function() {
      if (is.null(private$.flows)) {
        private$.flows = OMLContainer$new(
          mlr3misc::map(self$flow_ids, function (x) OMLFlow$new(x))
        )
      }
      return(private$.flows)
    },

    #' @field data (`list(n)')
    #' A list of [mlr3oml][mlr3oml::OMLData].
    data = function() {
      if (is.null(private$.data)) {
          private$.data = OMLContainer$new(
            mlr3misc::map(self$data_ids, function (x) OMLData$new(x))
          )
      }
      return(private$.data)
    },

    #' @field data (`list(n)')
    #' A list of [mlr3oml][mlr3oml::OMLData].
    tasks = function() {
      if (is.null(private$.tasks)) {
        private$.tasks = OMLContainer$new(
          mlr3misc::map(self$task_ids, function (x) OMLTask$new(x))
        )
      }
      return(private$.tasks)
    }
  ),

  private = list(
    .desc = NULL,
    .runs = NULL,
    .tasks = NULL,
    .data = NULL,
    .flows = NULL
  )
)

#map_progress = function(xs, f) {
#  p = progressr::progressor(along = xs)
#  y = mlr3misc::map(xs, function(x) {
#    p()
#    f(x)
#  })
#  return(y)
#}
#private$.tasks = progressr::with_progress(
#  map_progress(self$task_ids, function (x) OMLTask$new(x))
