#' @title Interface to OpenML Runs
#'
#' @description
#' This is the class for OpenML [Runs](https://new.openml.org/search?type=run).
#'  * A [OMLTask] is returned by the active field `$task`.
#'  * A [OMLData] is returned by the active field `$data` (short for $task$data)
#'  * A [OMLFlow] is returned by the active field `$flow`.
#'
#' @section mlr3 Integration:
#' A [ResampleResult] is obtained by calling the method `$convert()`
#'
#' @references
#' `r format_bib("vanschoren2014")`
#'
#' @export
#' @examples
#' \donttest{
#' oml_run = OMLRun$new(id = 1)
#'
#' print(oml_run)
#' print(oml_run$task) # OMLTask
#' print(oml_run$data) # OMLData
#' print(oml_run$flow) # OMLFlow
#' print(oml_run$resampling) # OMLResampling
#' }
#'
OMLRun = R6Class("OMLRun",
  public = list(
    #' @field id (`integer(1)`)\cr
    #'  OpenML run id.
    id = NULL,

    #' @template field_cache_dir
    cache_dir = NULL,


    #' @description
    #'  Creates a new object of class `OMLRun`.
    #'
    #' @param id (`integer(1)`) \cr
    #'  OpenML run id.
    #' @template param_cache
    initialize = function(id, cache = getOption("mlr3oml.cache", FALSE)) {
      # Neither run_id nor cache_dir should be changed after initialization
      self$id = assert_count(id, coerce = TRUE)
      self$cache_dir = get_cache_dir(assert_flag(cache))
      initialize_cache(self$cache_dir)
    },

    #' @description
    #' Prints the object.
    print = function() {
      catf("<OMLRun:%i>", self$id)
    },

    #' @description
    #' Converts the OMLRun into a [mlr3::ResampleResult].
    #' In case the flow is not a mlr3 learner, a pseudo learner is created (see [OMLFlow])
    #' and the resamplign result can still be used for some operations.
    #' @param store_backends (`logical(1)`) Whether to store the backends
    convert = function(store_backends = TRUE) {
      task = self$task$convert()
      resampling = self$task$resampling$convert()
      iterations = resampling$iters

      # convert the predictions into mlr3-readable format
      predictions = split_predictions(
        self$prediction, self$task$resampling$convert(),
        self$task_type
      )
      n = length(predictions)
      learner = self$flow$convert(self$task_type)
      learners = map(seq(n), function(x) learner$clone(deep = TRUE))

      states = tryCatch(
        # mlr or mlr3
        get_rds(self$desc$output_data$file$url[self$desc$output_data$file$name == "binary"]),
        error = function(x) {
          # two problematic cases: some Weka learners have duplicate parameter names and sometimes
          # the parameter_setting of a run does not match the parameters of the flow
          valid_params = test_subset(self$parameter_setting$name, self$flow$parameter$name) &&
            length(unique(self$parameter_setting$name)) == length(self$parameter_setting$name)
          if (valid_params) {
            param_vals = set_names(
              x = self$parameter_setting[["value"]],
              nm = make.names(self$parameter_setting[["name"]])
            )
          } else {
            warning("Problematic parameter setting, setting all parameters to NA.")
            param_vals = set_names(
              x = as.list(rep(NA, learners[[1]]$param_set$length)),
              learners[[1]]$param_set$ids()
            )
          }
          states = map(seq_len(n), function(x) {
            state = list(
              param_vals = param_vals,
              task_hash = task$hash,
              mlr3_version = NA_character_,
              task_prototype = task$data(rows = integer())
            )
            if (store_backends) {
              state$train_task = task
            }
            return(state)
          })
          return(states)
        }
      )
      pmap(list(learners, states),
        function(learner, state) {
          learner$param_set$values = state$param_vals
        }
      )
      data = data.table(
        task = list(task),
        learner = learners,
        learner_state = states,
        resampling = list(resampling),
        iteration = seq_len(resampling$iters),
        prediction = predictions,
        uhash = uuid::UUIDgenerate()
      )

      rr = mlr3::ResampleResult$new(mlr3::ResultData$new(data, store_backends = store_backends))
      return(rr)
    }
  ),
  active = list(
    #' @field desc (`list()`)
    #' Run description (meta information) downloaded and converted from
    #' the JSON API response.
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = cached(download_run_desc, "run_desc", self$id,
          cache_dir = self$cache_dir
        )
      }
      return(private$.desc)
    },

    #' @field flow_id (`integer(1)`)\cr
    #'  The id of the flow.
    flow_id = function() self$desc$flow_id,

    #' @field flow (OMLFlow)[OMLFlow] \cr
    #'  The OpenML Flow.
    flow = function() {
      if (is.null(private$.flow)) {
        private$.flow = OMLFlow$new(self$flow_id, cache = is.character(self$cache_dir))
      }
      private$.flow
    },

    #' @field task_id (`character(1)`)\cr
    #' The id of the task solved by this run.
    task_id = function() self$desc$task_id,

    #' @field task (`character(1)`)\cr
    #' The task solved by this run.
    task = function() {
      if (is.null(private$.task)) {
        private$.task = OMLTask$new(self$task_id, is.character(self$cache_dir))
      }
      private$.task
    },

    #' @field data ([OMLData]) \cr
    #' The data used in this run.
    data = function() self$task$data,

    #' @field data_id (`character(1)`)\cr
    #' The data id.
    data_id = function() self$desc$input_data$dataset$did,

    #' @field task_type (`character(1)`)\cr
    #' The type of task solved by this run (e.g., classification).
    task_type = function() self$desc$task_type,

    #' @field tags ('character(n)') \cr
    #' A character vector containing possible tags.
    tags = function() self$desc$tag,

    # #' @field evaluation
    # #' The evaluation.
    # evaluation = function() self$desc$output_data$evaluation,

    #' @field parameter_setting ('data.table') \cr
    #' The parameter setting for this run.
    parameter_setting = function() self$desc$parameter_setting,

    #' @field prediction (Prediction)[mlr3::Prediction]
    #' The prediction of the run.
    prediction = function() {
      if (!is.null(private$.prediction)) {
        return(private$.prediction)
      }

      is_prediction = grepl("prediction", self$desc$output_data$file$name)
      assert(sum(is_prediction) == 1L)
      private$.prediction = get_arff(self$desc$output_data$file$url[is_prediction])
      return(private$.prediction)
    }
  ),
  private = list(
    .desc = NULL,
    .task = NULL,
    .prediction = NULL,
    .data = NULL,
    .flow = NULL
  )
)


#' splits the predictions from OpenML into mlr3 readable forat
split_predictions = function(predictions, resampling, task_type) {
  classes = c("PredictionDataClassif", "PredictionData")
  names = colnames(predictions)
  predictions = lapply(
    resampling$instance$test,
    # Weka, sklearn and mlr(3) have different formats for uploading predictions,
    # here we capture some (probably not all formats) and also discard unknown columns
    function(x) {
      if (test_subset(c("row_ids", "truth", "response"), names)) { # mlr3
        test_data = predictions[row_ids %in% x, c("row_ids", "truth", "response")]
      } else if (test_subset(c("row_ids", "truth", "reponse", "se"), names)) {
        test_data = predictions[row_ids %in% x, c("row_ids", "truth", "response", "se")]
      } else if (test_subset(c("row_id", "truth", "prediction"), names)) {
        test_data = predictions[row_id %in% x, c("row_id", "truth", "prediction")]
        colnames(test_data) = c("row_ids", "truth", "response")
      } else if (test_subset(c("row_id", "correct", "prediction"), names)) {
        test_data = predictions[row_id %in% x, c("row_id", "correct", "prediction")]
        colnames(test_data) = c("row_ids", "truth", "response")
      } else {
        stop("Could not parse prediction.")
      }
      test_data = as.list(test_data)
      class(test_data) = classes
      list(test = test_data)
    }
  )

  return(predictions)
}
