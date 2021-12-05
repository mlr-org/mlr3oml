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
    task_id = NULL,
    cache_dir = NULL,
    initialize = function(id, cache = getOption("mlr3oml.cache", FALSE)) {
      self$task_id = assert_count(id, coerce = TRUE)
      self$cache_dir = get_cache_dir(assert_flag(cache))
      initialize_cache(self$cache_dir)
    }
    #convert = function() {
    #  train_sets = self$splits[type == "TRAIN", list(row_id = list(as.integer(rowid) + 1L)),
    #    keyby = c("repeat.", "fold")]$row_id
    #  test_sets = self$splits[type == "TEST", list(row_id = list(as.integer(rowid) + 1L)),
    #    keyby = c("repeat.", "fold")]$row_id

    #  resampling = mlr3::ResamplingCustom$new()
    #  instance = resampling$instantiate(
    #    task = OMLTask$new(self$task_id)$convert(),
    #    train_sets = train_sets,
    #    test_sets = test_sets)
    #  return(instance)
    #}

  ),
  active = list(
    splits = function() {
      if (is.null(private$.splits)) {
        private$.splits = cached(download_task_splits, "task_splits", self$task_id, self$desc,
          cache_dir = self$cache_dir)
      }

      private$.splits
    },
    type = function() {
      # FIXME:
      if (is.null(private$.type)) {
        private$.type = retrieve_resampling_type(self$split)
      }

      return(private$.type)
    },
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = download_task_desc(self$task_id)
      }

      return(private$.desc)
    },
    ep = function() {
      return(private$.desc$estimation_procedure)
    }
  ),

  private = list(
    .type = NULL,
    .desc = NULL,
    .ep = NULL,
    .splits = NULL
  )
)


retrieve_resampling_type = function(split) {
  # TODO: implement this
  return("Unknown")
}
