OMLTaskConnector = function(task_id = NULL, data_id = NULL, target_names = NULL,
  cache = getOption("mlr3oml.cache", FALSE)) {
  if (!xor(is.null(task_id), is.null(data_id))) {
    stop("Either 'task_id' (x)or 'data_id' must be provided")
  }

  if (!is.null(task_id)) {
    OMLTask$new(task_id, cache)$task
  } else {
    OMLData$new(data_id, cache)$task(target_names)
  }
}
