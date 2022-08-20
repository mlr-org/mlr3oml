OMLTaskConnector = function(task_id = NULL, data_id = NULL, target_names = NULL,
  cache = getOption("mlr3oml.cache", FALSE), parquet = getOption("mlr3oml.parquet", FALSE)) {
  if (!xor(is.null(task_id), is.null(data_id))) {
    stop(errorCondition("Either 'task_id' (x)or 'data_id' must be provided", class = "missingDefaultError"))
  }

  if (!is.null(task_id)) {
    as_task(OMLTask$new(task_id, cache, parquet), target_names)
  } else {
    as_task(OMLData$new(data_id, cache, parquet), target_names)
  }
}
