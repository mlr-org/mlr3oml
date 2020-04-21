OMLResamplingConnector = function(task_id = NULL, use_cache = getOption("mlr3oml.use_cache", FALSE)) {
  if (is.null(task_id)) {
    stop("Argument 'task_id' must be provided")
  }

  OMLTask$new(task_id, use_cache)$resampling
}
