#' @title Interface to OpenML Splits
#'
#' @description
#' This is the class for OpenML splits that belong to task.
#'
#' @section mlr3Intergration:
#' A (instantiated) [mlr3::Resampling] can be obtained using the method `$convert()`.
#'
#'
#' @examples
#' \donttest{
#' otask = OMLTask$new(59) # mlr3oml::OMLTask
#' oresampling = otask$resampling # [mlr3oml::OMLResampling]
#' resampling = oresampling$convert() # mlr3::Resampling
#' }
#' @export
OMLResampling = R6Class("OMLResampling",
  public = list(
    #' @field id (`integer(1)`)
    #' The id of the task to which the resampling belons.
    id = NULL,
    #' @field task (`OMLTask`) to which the resampling belongs.
    task = NULL,
    #' @template field_cache_dir
    cache_dir = NULL,
    #' @description
    #' Initializes an  instance of class OMLResampling. Requires either
    #' @param task (`OMLTask) Either the OpenML Task or task_id have to be provided.
    #' @param task_id (`integer(1)`) The OpenML task ID (if no task is provided).
    #' @template param_cache
    initialize = function(task = NULL, task_id = NULL, cache = getOption("mlr3oml.cache", FALSE)) {
      assert(is.null(task) || is.null(task_id))
      assert_true(is.null(task) || inherits(task, "OMLTask"))
      if (is.null(task_id)) {
        assert_r6(task, "OMLTask")
        self$task = task
        self$id = task$id
      } else {
        self$task = OMLTask$new(task_id, cache = cache)
        self$id = task_id
      }
      # TODO: Does the caching work properly in this case? --> check, we don't want to
      # unnecessarily download the task description twice for the resmapling and the task
      self$cache_dir = get_cache_dir(assert_flag(cache))
      initialize_cache(self$cache_dir)
    },
    #' @description
    #' Returns an instantiated [mlr3::Resampling] object.
    convert = function() {
      if (is.null(private$.resampling)) {
        splits = cached(download_task_splits, "task_splits", self$id, self$task$desc,
          cache_dir = self$cache_dir
        )

        train_sets = splits[type == "TRAIN", list(row_id = list(as.integer(rowid) + 1L)),
          keyby = c("repeat.", "fold")
        ]$row_id
        test_sets = splits[type == "TEST", list(row_id = list(as.integer(rowid) + 1L)),
          keyby = c("repeat.", "fold")
        ]$row_id

        resampling = mlr3::ResamplingCustom$new()
        private$.resampling = resampling$instantiate(self$task,
          train_sets = train_sets,
          test_sets = test_sets
        )
      }

      private$.resampling
    }
  ),
  active = list(
    #' @field estimation_procedure (`character(1)`) The OpenML estimation procedure.
    estimation_procedure = function() self$task$desc$input$estimation_procedure$type
  ),
  private = list(
    .resampling = NULL,
    .url = NULL
  )
)
