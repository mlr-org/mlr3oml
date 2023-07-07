OMLResamplingConnector = function(task_id = NULL, test_server = test_server_default(), parquet = parquet_default()) {
  if (is.null(task_id)) {
    stop(errorCondition("Argument 'task_id' must be provided", class = "missingDefaultError"))
  }
  as_resampling(OMLTask$new(task_id, test_server = test_server, parquet = parquet))
}
