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

transpose_name_value = function(li, as_integer = FALSE) {
  converter = if (as_integer) as.integer else as.numeric
  tab = rbindlist(map(li, function(x) {
    if (!is.null(x) && all(dim(x))) {
      set_names(as.list(converter(x$value)), x$name)
    } else {
      data.table(..dummy = 1)
    }
  }), fill = TRUE)

  remove_named(tab, "..dummy")
}

# remove this when it is merged in mlr3db
rename_duckdb_backend = function(backend) {
  old = backend$colnames
  new = make.names(old, unique = TRUE)

  existing_tables = DBI::dbGetQuery(get_private(backend)$.data, "PRAGMA show_tables")$name
  table_new = make.unique(c(existing_tables, backend$table), sep = "_")[length(existing_tables) + 1L]
  primary_key_new = new[old == backend$primary_key]
  tmp_old = paste0("\"", old, "\"")
  tmp_new = paste0("\"", new, "\"")
  renamings = paste(tmp_old, "AS", tmp_new, collapse = ", ")
  query = sprintf('CREATE VIEW "%s" AS SELECT %s from "%s"', table_new, renamings, backend$table)

  DBI::dbExecute(get_private(backend)$.data, query)

  backend$table = table_new
  backend$primary_key = primary_key_new

  invisible(backend)
}

as_duckdb_backend_character = function(data, path = getOption("mlr3db.duckdb_dir", ":temp:"), primary_key = NULL, ...) {
  assert_file_exists(data, access = "r", extension = "parquet")
  con = DBI::dbConnect(duckdb::duckdb())

  query = "CREATE OR REPLACE VIEW 'mlr3db_view' AS SELECT *"
  if (is.null(primary_key)) {
    primary_key = "mlr3_row_id"
    query = paste0(query, ", row_number() OVER () AS mlr3_row_id")
  } else {
    assert_string(primary_key)
  }

  query = sprintf("%s FROM parquet_scan(['%s'])", query, paste0(data, collapse = "','"))
  DBI::dbExecute(con, query)

  DataBackendDuckDB$new(con, table = "mlr3db_view", primary_key = primary_key, ...)

}

