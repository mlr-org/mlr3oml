#' @title Interface to OpenML Runs
#'
#' @name oml_run
#'
#' @description
#' This is the class for OpenML [Runs](https://openml.org/search?type=run), which are
#' conceptually analogous to [mlr3::ResampleResult]s.
#'
#' @section OpenML Integration:
#' * A [OMLTask] is returned by accessing the active field `$task`.
#' * A [OMLData] is returned by accessing the active field `$data` (short for `$task$data`)
#' * A [OMLFlow] is returned by accessing the active field `$flow`.
#' * The raw predictions are returned by accessing the active field `$predictions`.
#'
#' @section mlr3 Integration:
#' * A [mlr3::ResampleResult] is returned when calling `as_resample_result()`.
#' * A [mlr3::Task] is returned when calling `as_task()`.
#' * A [mlr3::DataBackend] is returned when calling `as_data_backend()`.
#' * A [mlr3::Learner] is returned when calling `as_learner()`.
#' * A instantiated [mlr3::Resampling] is returned when calling `as_resampling()`.
#'
#' @references
#' `r format_bib("vanschoren2014")`
#'
#' @export
#' @examples
#' \donttest{
#' library("mlr3")
#' orun = OMLRun$new(id = 10587724)
#' print(orun)
#' print(orun$task) # OMLTask
#' print(orun$data) # OMLData
#' print(orun$flow) # OMLFlow
#' print(orun$prediction)
#' as_task(orun)
#' as_resampling(orun)
#' as_learner(orun)
#' as_data_backend(orun)
#' rr = as_resample_result(orun)
#' rr$score(msr("classif.ce"))
#' }
#' #
OMLRun = R6Class("OMLRun",
  inherit = OMLObject,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_id
    #' @template param_cache
    #' @template param_parquet
    #' @template param_test_server
    initialize = function(
      id,
      cache = getOption("mlr3oml.cache", FALSE),
      parquet = getOption("mlr3oml.parquet", FALSE),
      test_server = getOption("mlr3oml.test_server", FALSE)
      ) {
      super$initialize(id, cache, parquet, test_server, "run")
    },
    #' @description
    #' Prints the object.
    print = function() {
      catf("<OMLRun:%i>", self$id)
      catf(" * Task: %s (id: %s)", as_short_string(self$task$data_name), self$task_id)
      ep = self$task$estimation_procedure
      catf(" * Flow: %s (id: %s)", as_short_string(self$flow$name), self$flow_id)
      catf_estimation_procedure(self$task$estimation_procedure)
      if (self$test_server) {
        catf(" * Using test server")
      }
    }
  ),
  active = list(
    #' @field flow_id (`integer(1)`)\cr
    #'  The id of the flow.
    flow_id = function() self$desc$flow_id,
    #' @field flow ([OMLFlow])\cr
    #'  The OpenML Flow.
    flow = function() {
      if (is.null(private$.flow)) {
        private$.flow = OMLFlow$new(self$flow_id, cache = self$cache_dir,
          test_server = self$test_server, parquet = self$parquet
        )
      }
      private$.flow
    },
    #' @field task_id (`character(1)`)\cr
    #' The id of the task solved by this run.
    task_id = function() self$desc$task_id,
    #' @field task ([OMLTask])\cr
    #' The task solved by this run.
    task = function() {
      if (is.null(private$.task)) {
        private$.task = OMLTask$new(self$task_id, self$cache_dir,
          test_server = self$test_server, parquet = self$parquet
        )
      }
      private$.task
    },
    #' @field data_id (`integer(1)`)\cr
    #' The id of the dataset.
    data_id = function() self$desc$input_data$dataset$did,
    #' @field data ([OMLData]) \cr
    #' The data used in this run.
    data = function() self$task$data,
    #' @field task_type (`character()`)\cr
    #' The task type.
    task_type = function() self$desc$task_type,
    #' @field parameter_setting `data.table()`)\cr
    #' The parameter setting for this run.
    parameter_setting = function() self$desc$parameter_setting,
    #' @field prediction (`data.table()`)\cr
    #' The raw predictions of the run as returned by OpenML, not in standard mlr3 format.
    #' Formatted predictions are accessible after converting to a [mlr3::ResampleResult] via
    #' `as_resample_result()`.
    prediction = function() {
      if (!is.null(private$.prediction)) {
        return(private$.prediction)
      }
      private$.prediction = cached(download_prediction, "prediction", self$id,
        cache_dir = self$cache_dir, desc = self$desc, test_server = self$test_server
      )
      return(private$.prediction)
    }
  ),
  private = list(
    .task = NULL,
    .prediction = NULL,
    .data = NULL,
    .flow = NULL,
    .task_split = NULL
  )
)


#' @title Splits the predictions from OpenML into mlr3 readable format.
#' @description Returns a list, where each item is a list with the elements row_id, truth,
#' prediction and in case the predict_type is 'prob' an element prob containing the probability
#' matrix.
#'
#'
#' @param predictions (`data.table()` The `$prediction` field of a run.)
#' @param resampling (mlr3::Resampling). Result of calling as_resampling(otask).
#' @param task_type (`character(1)`). Result of calling `otask$task_type`.
#'
split_predictions = function(predictions, resampling, task_type) {
  if (task_type == "Supervised Classification") {
    classes = c("PredictionDataClassif", "PredictionData")
  } else if (task_type == "Supervised Regression") {
    classes = c("PredictionDataRegr", "PredictionData")
  } else if (task_type == "Survival Analysis") {
    stopf("mlr3proba currently not supported")
    # require_namespaces("mlr3proba")
    # classes = c("PredictionDataSurv", "PredictionData")
  } else {
    stop("Unsupported Task type.")
  }
  names = colnames(predictions)
  test_sets = map(seq(resampling$iters), function(i) resampling$test_set(i))
  colnames(predictions)[colnames(predictions) == "row_ids"] = "row_id"

  predictions = lapply(
    test_sets,
    # Weka, sklearn and mlr(3) have different formats for uploading predictions,
    # here we capture some (probably not all formats) and also discard unknown columns
    function(x) {
      if (test_subset(c("row_ids", "truth", "reponse", "se"), names)) { # mlr3
        test_data = predictions[get("row_id") %in% x, c("row_ids", "truth", "response", "se")]
      } else if (test_subset(c("row_ids", "truth", "response"), names)) { # mlr3
        test_data = predictions[get("row_id") %in% x, c("row_ids", "truth", "response")]
      } else if (test_subset(c("row_id", "truth", "prediction"), names)) {
        test_data = predictions[get("row_id") %in% x, c("row_id", "truth", "prediction")]
        colnames(test_data) = c("row_ids", "truth", "response")
      } else if (test_subset(c("row_id", "correct", "prediction"), names)) {
        test_data = predictions[get("row_id") %in% x, c("row_id", "correct", "prediction")]
        colnames(test_data) = c("row_ids", "truth", "response")
      } else {
        stop("Could not parse prediction.")
      }
      test_data = as.list(test_data)
      if (task_type == "Supervised Classification") {
        prob_names = colnames(predictions)[startsWith(colnames(predictions), "confidence.")]
        probs = predictions[get("row_id") %in% x, prob_names, with = FALSE]
        probs_are_ints = all(map_lgl(probs, is.integer)) # only zeros and ones
        if (!probs_are_ints) { # 1 0 are no proper probabilities.
          colnames(probs) = gsub("confidence.", "", prob_names)
          test_data$prob = probs
        }
      }
      class(test_data) = classes
      list(test = test_data)
    }
  )
  return(predictions)
}

#' @importFrom mlr3 as_task
#' @export
as_task.OMLRun = function(x, ...) {
  as_task(x$task, ...)
}

#' @importFrom mlr3 as_learner
#' @export
as_learner.OMLRun = function(x, task_type = NULL, ...) {
  if (is.null(task_type)) {
    task_type = task_type_translator(x$task_type, to = "mlr3")
  }
  as_learner(x$flow, task_type = task_type, ...)
}

#' @importFrom mlr3 as_data_backend
#' @export
as_data_backend.OMLRun = function(x, ...) {
  as_data_backend(x$data, ...)
}

#' @importFrom mlr3 as_resampling
#' @export
as_resampling.OMLRun = function(x, ...) {
  as_resampling(x$task, ...)
}

#' @importFrom mlr3 as_resample_result
#' @export
as_resample_result.OMLRun = function(x, store_backends = TRUE, ...) {
  task = as_task(x, ...)
  resampling = as_resampling(x, task = task, ...)
  flow = x$flow
  iterations = resampling$iters

  # convert raw predictoins into mlr3 PredictionData
  predictions = split_predictions(x$prediction, resampling, x$task_type)
  n = length(predictions)
  learner = as_learner(flow, task$task_type, ...)
  learners = map(seq(n), function(x) learner$clone(deep = TRUE))


  param_vals = set_names(
    x = x$parameter_setting[["value"]],
    nm = make.names(
      paste0("f", x$parameter_setting[["component"]], ".", x$parameter_setting[["name"]])
    )
  )
  if (!all(names(param_vals) %in% learner$param_set$ids())) {
    warningf("Problem assigning parameter_setting to learner, setting all parameter values to NA.")
    param_vals = set_names(
      x = as.list(rep(NA, learner$param_set$length)),
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

  rr = ResampleResult$new(ResultData$new(data, store_backends = store_backends))
  return(rr)
}
