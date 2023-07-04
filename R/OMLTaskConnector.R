OMLTaskConnector = function(task_id = NULL, data_id = NULL, target_names = NULL,
  parquet = parquet_default(), test_server = test_server_default()) {
  if (!xor(is.null(task_id), is.null(data_id))) {
    stop(errorCondition("Either 'task_id' (x)or 'data_id' must be provided", class = "missingDefaultError"))
  }

  if (!is.null(task_id)) {
    as_task(
      OMLTask$new(task_id, parquet = parquet, test_server = test_server),
      target_names
    )
  } else {
    as_task(
      OMLData$new(data_id, parquet = parquet, test_server = test_server),
      target_names
    )
  }
}
