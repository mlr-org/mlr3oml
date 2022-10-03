OMLResamplingConnector = function(task_id = NULL, cache = cache_default(),
  test_server = test_server_default(), parquet = parquet_default()) {
  if (is.null(task_id)) {
    stop(errorCondition("Argument 'task_id' must be provided", class = "missingDefaultError"))
  }
  as_resampling(OMLTask$new(task_id, cache = cache, test_server = test_server, parquet = parquet))
}
