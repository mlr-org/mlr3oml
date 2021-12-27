#' @title Interface to OpenML Splits
#'
#' @description
#' This is the class for OpenML splits that belong to task.
#'
#' @section mlr3Intergration:
#' Intermediary object that can be converted to mlr3::Resampling.
#'
#' @export
#'
#' @examples
#' \donttest{
#' otask = OMLTask$new(59) # mlr3oml::OMLTask
#' oresampling = otask$resampling # [mlr3oml::OMLResampling]
#' resampling = oresampling$convert() # mlr3::Resampling
#' }
#'
OMLResampling = R6Class("OMLResampling",
  public = list(
    id = NULL,
    task = NULL,
    estimation_procedure = NULL,
    cache_dir = NULL,
    initialize = function(task, cache = getOption("mlr3oml.cache", FALSE)) {
      assert_r6(task, "OMLTask")
      self$task = task
      self$id = task$id
      self$estimation_procedure = task$desc$input$estimation_procedure
      self$cache_dir = get_cache_dir(assert_flag(cache))
      initialize_cache(self$cache_dir)
    },
    convert = function() {
      if (is.null(private$.resampling)) {
        splits = cached(download_task_splits, "task_splits",
          self$estimation_procedure, cache_dir = self$cache_dir)

        train_sets = splits[type == "TRAIN", list(row_id = list(as.integer(rowid) + 1L)),
          keyby = c("repeat.", "fold")]$row_id
        test_sets = splits[type == "TEST", list(row_id = list(as.integer(rowid) + 1L)),
          keyby = c("repeat.", "fold")]$row_id

        resampling = mlr3::ResamplingCustom$new()
        private$.resampling = resampling$instantiate(self$task, train_sets = train_sets,
          test_sets = test_sets)
      }

      private$.resampling
    }
  ),
  private = list(
    .resampling = NULL,
    .url = NULL
  )
)
