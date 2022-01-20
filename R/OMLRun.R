#' @title Interface to OpenML Runs
#'
#' @description
#' This is the class for tasks provided on \url{https://openml.org/}.
#'
#' @section mlr3 Integration:
#' A [mlr3oml::OMLTask] is returned by the active field `$task`.
#' A [mlr3oml::OMLData] is returned by the active field `$data` (short for $task$data)
#' A [mlr3oml::OMLFlow] is returned by the active field `$flow`.
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
#' print(oml_run$resampling) # OMLFlow
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
    #' Converts the OMLRun into a (Resampling)[mlr3::Resampling] object.
    convert = function(store_backends = TRUE) {
      assert("binary" %in% self$desc$output_data$file$name)
      states = get_rds(
        self$desc$output_data$file$url[self$desc$output_data$file$name == "binary"]
      )
      learners = replicate(length(states), self$flow$convert())
      .f = function(learner, state) {
        learner$param_set$values = state$param_vals
      }
      pmap(list(learners, states), .f)
      predictions = split_predictions(
        self$prediction, self$task$resampling$convert(),
        self$task_type
      )
      resampling = self$task$resampling$convert()
      iterations = resampling$iters
      task = self$task$convert()
      data = data.table(
        task = list(task),
        learner = learners,
        learner_state = states,
        resampling = list(resampling),
        iteration = seq_len(resampling$iters),
        prediction = predictions,
        uhash = uuid::UUIDgenerate()
      )

      ResampleResult$new(ResultData$new(data, store_backends = store_backends))
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

    #' @field flow (OMLFlow)[mlr3::OMLFlow] \cr
    #'  The [mlr3oml::OMLFlow].
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

    #' @field tag ('character(n)') \n
    #' A character vector containing possible tags.
    tag = function() self$desc$tag,

    #' @field evaluation
    #' The evaluation.
    evaluation = function() self$desc$output_data$evaluation,

    #' @field  ('data.table') \n
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


split_predictions = function(predictions, resampling, task_type) {
  # classes = switch(task_type,
  #   "classif" = c("PredictionDataClassif", "PredictionData"),
  #   "regr" = c("PredictionDataRegr", "PredictionData")
  # )
  classes = c("PredictionDataClassif", "PredictionData")
  predictions = lapply(
    resampling$instance$test,
    function(x) {
      test_data = as.list(predictions[row_ids %in% x, ])
      class(test_data) = classes
      list(test = test_data)
    }
  )

  return(predictions)
}
