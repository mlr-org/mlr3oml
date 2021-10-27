#' @title Interface to OpenML Runs
#' @description
#' This is the class for OpenML runs TODO: Add link of new website when done.
#'
#' @section mlr3 Integration:
#' Blablab
#'
#' @references
#' `r format_bib("vanschoren2014")`
#'
#' @export
#' @examples
#' \donttest{
#' orun = OMLRun$new(id = 1)
#' pprint(otask)
#' }
#'
OMLRun = R6Class("OMLRun",
  public = list(
    #' @field id (`integer(1)`)\cr
    #' OpenML run id.
    id = NULL,

    #' @template field_cache_dir
    cache_dir = NULL,

    #' @field flow (`integer(1)`)\cr
    #' flow.
    flow = NULL,



    #' @description
    #' Creates a new object of class `OMLRun`.
    #'
    #' @param id (`integer(1)`) \cr
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
      catf("<OMLTask:%i:%s>", self$id, self$name)
    }
  ),
  active = list(
    task = function() {

    }


  ),

  private = list(
    .desc = NULL
  )

  #  active = list(
  #    #' @field desc (`list()`)\cr
  #    #'   Run description (meta information), downloaded and converted from the JSON API response.
  #    desc = function() {
  #      if (is.null(private$.desc)) {
  #        private$.desc = cached(download_run_desc, "run_desc", self$id, cache_dir = self$cache_dir)
  #      }
  #
  #      private$.desc
  #    },
  #
  #
  #    task = function() {
  #      OMLTask$new(self$desc$task_id)
  #    }
  #  ),

  private = list(
    .desc = NULL
  )
)


# if (FALSE) {
#  options(mlr3oml.cache = TRUE)
#  orun = OMLRun$new(12)
#  # self = orun
#  orun$desc
#  task = orun$task
#
#
#
#  library(mlr3)
#  task = OMLTask$new(6)
#  l = lrn("classif.rpart")
#  resampling = task$resampling
#  rr = resample(task$task, l, resampling)
#
#  # fehlt:
#  tab = as.data.table(rr$prediction())
#  foreign::write.arff(tab, file = "/tmp/predictions.arff")
#  # -> upload
# }
