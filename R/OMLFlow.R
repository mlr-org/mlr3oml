#' @title Interface to OpenML Flows
#'
#' @description
#' This is the class for flows served on the [OpenML website](https://new.openml.org/search?type=flow).
#'
#' @section mlr3 Integration:
#' It can be converted to a [mlr3::Learner] by calling the method `$convert()`.
#'
#' @references
#' `r format_bib("vanschoren2014")`
#'
#' @export
#' @examples
#' \donttest{
#' flow = OMLFlow$new(id = 19081)
#' learner = flow$convert()
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
    #' Converts the flow into an [mlr3::Learner] if possible.
    #' In case the flow is not an [mlr3::Learner] a pseudo OpenML learner is constructed if
    #' for the provided task type. If no task type is given it returns NULL.
    #' @param task_type (`character(1)`) The task_type as described above.
    convert = function(task_type = NULL) {
      assert_character(task_type, null.ok = TRUE, len = 1, any.missing = FALSE)
      # For mlr3 learners, the binary version for the learner is uploaded
      learner = tryCatch(get_rds(self$desc$binary_url), error = function(e) NULL)
      if (is.list(learner)) { # mlr Flow
        message("For mlr learners please use the OpenML package.")
        learner = NULL
      }
      # If no binary is provided we require the task_type to be able to construct the pseudo
      # OpenML learner
      if (is.null(learner) && !is.null(task_type)) {
        if (task_type %nin% c("classif", "regr")) {
          task_type = task_type_translator[[task_type]]
        }
        learner = make_oml_learner(self, task_type)
      }

      if (!is.null(learner)) {
        learner$.__enclos_env__$private$oml_id = self$id
      } else {
        message("Could not convert flow to learner.")
      }
      return(learner)
    },

    #' @description
    #' Prints the object.
    print = function() {
      catf("<OMLFlow:%i>", self$id)
    }
  ),
  active = list(
    #' @field desc (`list()`)\cr
    #' The description as downloaded from OpenML.
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = download_flow_desc(self$id)
      }
      private$.desc
    },

    #' @field parameter (`data.table`)\cr
    #' The parameters of the flow.
    parameter = function() self$desc$parameter,

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
