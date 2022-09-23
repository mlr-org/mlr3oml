OMLResamplingConnector = function(task_id = NULL, cache = getOption("mlr3oml.cache", FALSE),
  test_server = getOption("mlr3oml.test_server", FALSE), parquet = getOption("mlr3oml.parquet", TRUE)) {
  if (is.null(task_id)) {
    stop(errorCondition("Argument 'task_id' must be provided", class = "missingDefaultError"))
  }
  as_resampling(OMLTask$new(task_id, cache = cache, test_server = test_server, parquet = parquet))
}
