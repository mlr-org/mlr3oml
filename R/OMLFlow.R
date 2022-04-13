#' @title Interface to OpenML Flows
#'
#' @description
#' This is the class for flows served on [OpenML](https://www.openml.org/search?type=flow&sort=runs).
#' Flows are conceptually similar to [mlr3pipelines::Graph]s.
#' The support for publishing learners on OpenML (as flows) is currently limited but can be
#' extended upon request.
#'
#' @section mlr3 Integration:
#' * Obtain a [mlr3::Learner] using `as_learner()`. mlr3 Flows are converted into proper mlr3
#' learners, while other flows are converted to non-executable pseudo-learners, where the
#' task type has to be passed to `as_task()` as OpenML Flows are task-agnostic. While these learners
#' are not directly useful, this allows sklearn runs to be converted to [mlr3::ResampleResult]s and
#' run collections can be converted to [mlr3::BenchmarkResult]s.
#' The parameters of the flow include the parameters of all subcomponents, where the name
#' c.123.par  means that this is the parameter 'par' of the component with id 123.
#'
#' @references
#' `r format_bib("vanschoren2014")`
#'
#' @export
#' @examples
#' \donttest{
#' library("mlr3")
#' # mlr3 flow:
#' flow = OMLFlow$new(id = 19082L)
#' learner = as_learner(flow)
#' # python flow
#' python_flow = OMLFlow$new(19090L)
#' # conversion to pseudo Learner
#' plearner = as_learner(python_flow, "classif")
#' }
OMLFlow = R6Class("OMLFlow",
  public = list(
    #' @field id (`integer(1)`)\cr
    #' OpenML flow id.
    id = NULL,

    #' @template field_cache_dir
    cache_dir = NULL,

    #' @description
    #' Initializes a new object of class [mlr3oml::OMLFlow].
    #' @param id (`integeger(1)`) The id of the Flow
    #' @param cache (`logical(1)`) whether to use caching.
    initialize = function(id, cache = getOption("mlr3oml.cache", FALSE)) {
      self$id = assert_count(id, coerce = TRUE)
      self$cache_dir = get_cache_dir(assert_flag(cache))
      initialize_cache(self$cache_dir)
    },

    #' @description
    #' Prints the object.
    print = function() {
      catf("<OMLFlow:%i>", self$id)
      catf(" * Name: %s", as_short_string(self$name))
      catf(" * Dependencies: %s", paste(self$desc$dependencies, collapse = ", "))
    }
  ),
  active = list(
    #' @field desc (`list()`)\cr
    #' The description as downloaded from OpenML.
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = cached(download_flow_desc, type = "flow", id = self$id,
          cache_dir = self$cache_dir
        )
      }
      private$.desc
    },

    #' @field parameters (`data.table`)\cr
    #' The parameters of the flow.
    parameters = function() self$desc$parameter,

    #' @field tags (`character()`)\cr
    #' The tags of the flow.
    tags = function() self$desc$tag,

    #' @field dependencies (`character()`)\cr
    #' The dependencies of the flow.
    dependencies = function() self$desc$dependencies,

    #' @field name (`character(1)`)\cr
    #' The name of the flow.
    name = function() self$desc$name
  ),
  private = list(
    .desc = NULL
  )
)

#' @title Convert an OpenML Flow to a mlr3 Learner
#'
#' @description
#'   If it is not an mlr3 Flow it creates a pseudo learner (if `task_type` is given, otherwise
#'   it returns NULL).
#'   If it is a mlr3 Flow it downloads the binary rds file and compares the dependencies of the
#'   flow with the installed packages and the running R version (gives informative print output if
#'   the verbose flag is set).
#'
#' @param x (OMLFlow) The OMLFlow that is converted to a mlr3::Learner.
#' @param task_type (`character(1)`)
#'    The task type to constrct a pseudo-learner. For more information see [mlr3oml::OMLFlow].
#' @param verbose (`logical(1)`) Whether to give informative print output
#' @param ... Additional arguments
#' @importFrom mlr3 as_learner
#' @export
as_learner.OMLFlow = function(x, task_type = NULL, verbose = TRUE, ...) {
  assert_choice(task_type, c("regr", "classif", "surv"), null.ok = TRUE)
  assert_flag(verbose)

  learner = make_oml_learner(x, task_type)
  # if (!startsWith(x$name, "mlr3.")) {
  #   learner = make_oml_learner(x, task_type)
  #   return(learner)
  # }
  #
  # learner = cached(download_flow_binary, "learner", x$id, cache_dir = x$cache_dir, desc = x$desc)
  # if (!check_dependencies(x, verbose)) {
  #   try({
  #     learner$.__enclos_env__$private$oml = list(id = x$id, info = "dependency_mismatch")
  #   })
  # } else {
  #   try({
  #     learner$.__enclos_env__$private$oml = list(id = x$id)
  #   })
  # }

  return(learner)
}
