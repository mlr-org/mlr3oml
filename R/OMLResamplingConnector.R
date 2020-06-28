OMLResamplingConnector = function(task_id = NULL, cache = getOption("mlr3oml.cache", FALSE)) {
  if (is.null(task_id)) {
    stop("Argument 'task_id' must be provided")
  }

  OMLTask$new(task_id, cache)$resampling
}
