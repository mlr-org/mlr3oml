#' @title Interface to OpenML Flows
#'
#' @name oml_flow
#'
#' @description
#' This is the class for flows served on [OpenML](https://www.openml.org/search?type=flow&sort=runs).
#' Flows represent machine learning algorithms.
#' This object can also be constructed using the sugar function [oflw()].
#'
#' @section mlr3 Integration:
#' * Obtain a [mlr3::Learner] using [mlr3::as_learner()].
#'
#' @references
#' `r format_bib("vanschoren2014")`
#'
#' @export
#' @template examples
OMLFlow = R6Class("OMLFlow",
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
      super$initialize(id, test_server, "flow")
    },
    #' @description
    #' Prints the object.
    print = function() {
      # trigger download first for better printing
      self$desc
      catf("<OMLFlow:%i>", self$id)
      catf(" * Name: %s", as_short_string(self$name))
      catf(" * Dependencies: %s", paste(self$desc$dependencies, collapse = ", "))
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
    #' @field parameter (`data.table`)\cr
    #' The parameters of the flow.
    parameter = function() self$desc$parameter,

    #' @field dependencies (`character()`)\cr
    #' The dependencies of the flow.
    dependencies = function() self$desc$dependencies,
    #' @field tags (`character()`)\cr
    #' Returns all tags of the object.
    tags = function() {
      self$desc$tag
    }
  )
)

#' @title Convert an OpenML Flow to a mlr3 Learner
#'
#' @description
#'   By default this function creates a Pseudo-Learner (that cannot be used for training or
#'   prediction) for the given task type. This enables the conversion of OpenML Runs to
#'   [mlr3::ResampleResult]s.
#'   This is well defined because each subcomponent (i.e. id) can only appear once in a Flow
#'    according to the OpenML docs.
#'
#' @param x (OMLFlow) The OMLFlow that is converted to a mlr3::Learner.
#' @param task_type (`character(1)`)
#'    The task type to constrct a pseudo-learner. For more information see [mlr3oml::OMLFlow].
#' @param ... Additional arguments.
#'
#' @importFrom mlr3 as_learner
#' @export
as_learner.OMLFlow = function(x, task_type = NULL, ...) {
  assert_choice(task_type, c("regr", "classif"))
  class_name = sprintf("Learner%sOML%i", capitalize(task_type), x$id)
  super_class = if(task_type == "regr") LearnerRegr else LearnerClassif

  learner = R6Class(class_name,
    inherit = super_class,
    public = list(
      initialize = function() {
        super$initialize(
          id = sprintf("oml.%s", x$id),
          param_set = paradox::ps()
        )
      }
    ),
    private = list(
      .train = function(task) {
        stop("This is only a pseudo learner and cannot be trained.")
      },
      .predict = function(task) {
        stop("This is only a pseudo learner and cannot be used for prediction.")
      }
    )
  )$new()
}
