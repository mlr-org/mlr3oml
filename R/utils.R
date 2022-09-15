read_parquet = function(path) {
  require_namespaces(c("duckdb", "DBI"))
  con = DBI::dbConnect(duckdb::duckdb())
  data = DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s');", path))
  DBI::dbDisconnect(con, shutdown = TRUE)
  return(data)
}

get_desc_downloader = function(type) {
  switch(type,
    collection = download_desc_collection,
    study = download_desc_collection,
    run = download_desc_run,
    data = download_desc_data,
    task = download_desc_task,
    run = download_desc_run,
    flow = download_desc_flow,
    stopf("Invalid type '%s'.", type)
  )
}

get_server = function(test_server) {
  if (test_server) "https://test.openml.org/api/v1" else "https://www.openml.org/api/v1"
}

catf_estimation_procedure = function(estimation_procedure) {
  if (!is.null(estimation_procedure)) {
    type = estimation_procedure$type
    parameter = estimation_procedure$parameter
    if (type == "crossvalidation") {
      catf(" * Estimation: crossvalidation (id: %s; repeats: %s, folds: %s)",
        estimation_procedure$id,
        parameter[get("name") == "number_repeats", "value"][[1L]],
        parameter[get("name") == "number_folds", "value"][[1L]]
      )
    } else if (type == "holdout") {
      catf(" * Type: holdout (id: %s; test size: %s)",
        estimation_procedure$id,
        parameter[get("name") == "percentage", "value"][[1L]]
      )
    } else if (type == "leaveoneout") {
      catf(" * Type: leaveoneout (id: %s)", estimation_procedure$id)
    }
  } else {
    catf(" * Estimation: missing")
  }
}
