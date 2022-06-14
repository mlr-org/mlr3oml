# first checks the features whether there is a row identifier.
# If there is none it checks ..row_id, ...row_id, ... (whether they already exist).
get_row_identifier = function(con, features, table_info, tries = 10) {
  row_identifier = features[is_row_identifier == TRUE, "name"]

  if (nrow(row_identifier) == 1L) {
    return(row_identifier[[1L]])
  } else if (nrow(row_identifier) > 1L) {
    messagef("Multiple row_identifier detected, creating new row_identifier.")
  }

  row_identifier = "row_id__"

  for (i in seq_len(tries)) {
    if (row_identifier %nin% table_info$name) {
      break
    }
    row_identifier = sprintf("%s_", row_identifier)
  }
  if (row_identifier %in% table_info$name) {
    stopf("Tried to create row id column with names row_id__, row_id___, ... but they already exist.")
  }

  row_identifier
}

# this is a cached function
download_parquet = function(url, id, cache_dir = FALSE, path = NULL, api_key = get_api_key(),
  retries = 3L) {
  lg$info("Retrieving parquet", url = url, authenticated = !is.na(api_key))
  type = "data_pq"

  if (isTRUE(cache_dir)) {
    assert_true(is.null(path))
    path = file.path(cache_dir, type, sprintf("%s.pq", id))

    if (file.exists(file)) {
      lg$debug("Object exists in cache.", type = type)
      return(path)
    } else {
      lg$debug("Storing object in cache.", type = type, id = id, file = file)
    }
  }

  assert_path_for_output(path, extension = "pq")

  con = DBI::dbConnect(duckdb::duckdb())
  for (retry in seq_len(retries)) {
    response = download_file(url, path, api_key = api_key)
    lg$debug("Start processing parquet file", path = path)

    if (response$ok) {
      return(path)
    }
  }
  download_error(response)
}

as_parquet_backend = function(data, api_key = get_api_key(), retries = 3L, cache_dir = FALSE,
  path = NULL, db_path = getOption("mlr3db.duckdb_dir", ":temp:")) {
  type = "data_pq"
  id = data$id
  url = data$desc$minio_url
  db_path = get_db_path(path, hash = odata$id, "duckdb")

  # path is only used in case cache_dir = FALSE, we need to pass it for testing
  if (isFALSE(cache_dir)) { # stores the parquet file in path
    path = path %??% tempfile(fileext = ".pq")
    download_parquet(url, id, path = path)
  } else {
    path = download_parquet(url, id, cache_dir = cache_dir)
  }

  on.exit({
    if (!is.null(con)) DBI::dbDisconnect(con, shutdown = TRUE)
  }, add = TRUE)

  on.exit({
    if (file.exists(path)) unlink(paste0(path, c("", ".wal", ".tmp"), recursive = TRUE))
  }, add = TRUE)

  con = DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)

  # create the database
  DBI::dbExecute(con, sprintf("CREATE TABLE tbl AS SELECT * FROM read_parquet('%s');", path))
  table_info = DBI::dbGetQuery(con, "PRAGMA table_info('tbl')")

  # we need to create a row id in the database in case none exists
  rowid = get_row_identifier(con, data$features, table_info)

  if (rowid %nin% table_info$name) {

    DBI::dbExecute(con, sprintf("ALTER TABLE tbl ADD COLUMN %s INTEGER", rowid))
    DBI::dbExecute(con, sprintf("UPDATE tbl SET %s = rowid + 1 where 1 == 1", rowid))
  }

  DBI::dbDisconnect(con)

  con = DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  nominals = data$features$name[data$features$data_type == "nominal"]
  backend = mlr3db::DataBackendDuckDB$new(con, "tbl", primary_key = rowid,
    strings_as_factors = nominals
  )

  # reg.finalizer(db@database_ref, function(e) file.remove(path), onexit = TRUE)

  on.exit()

  backend
}
