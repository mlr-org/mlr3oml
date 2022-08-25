task_type_translator = function(tt, to = "mlr3") {
  if (to == "mlr3") {
    converted = switch(tt,
      "Supervised Regression" = "regr",
      "Supervised Classification" = "classif",
      "Survival Analysis" = "surv",
      "Clustering" = "clust",
      NULL
    )
  }
  if (to == "oml") {
    converted = switch(tt,
      "regr" = "Supervised Regression",
      "classif" = "Supervised Classification",
      "surv" = "Survival Analysis",
      "clust" = "Clustering",
      NULL
    )
  }
  return(converted)
}



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
