# this is a cached function
download_parquet_cached = function(url, id, cache_dir = FALSE, api_key = get_api_key(),
  retries = 3L) {
  lg$info("Retrieving parquet.", url = url, authenticated = !is.na(api_key))
  type = "data_parquet"
  if (isFALSE(cache_dir)) {
    file = tempfile(fileext = ".parquet")
  } else {
    path = file.path(cache_dir, type)
    file = file.path(path, sprintf("%s.parquet", id))
    if (file.exists(file)) {
      lg$info("Returning parquet path from cache.", id = id, file = file)
      return(file)
    } else if (!file.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  }

  lg$debug("Starting download of parquet file.", type = type, id = id, file = file)

  for (retry in seq_len(retries)) {
    response = download_file(url, file, api_key = api_key)

    if (response$ok) {
      if (isFALSE(cache_dir)) {
        lg$debug("Downloaded parquet file and stored in temporary file.", file = file, url = url, id = id) # nolint
      } else {
        lg$debug("Downloaded parque file and stored in cache.", file = file, path = path, id = id, url = url) # nolint
      }
      normalize_names_parquet_inplace(file)
      lg$debug("Normalized parquet names.", path = file, url = url, id = id)
      return(file)
    } else if (retry < retries && response$http_code >= 500L) {
      delay = max(rnorm(1L, mean = 10), 0)
      lg$debug("Server busy, retrying in %.2f seconds", delay, try = retry)
      Sys.sleep(delay)
    }
  }
  download_error(response)
}

normalize_names_parquet_inplace = function(path) {
  require_namespaces(c("duckdb", "DBI"))
  con = DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, sprintf("CREATE TABLE tbl AS SELECT * FROM \"%s\";", path))
  names = DBI::dbGetQuery(con, "PRAGMA table_info('tbl')")$name
  names_new = make.names(names)
  if (anyDuplicated(names_new)) {
    stop("Duplicated column names detected after conversion.")
  }
  for (i in seq_along(names)) {
    DBI::dbExecute(con, sprintf("ALTER TABLE tbl RENAME \"%s\" to \"%s\";", names[i], names_new[i]))
  }
  DBI::dbExecute(con, sprintf("COPY tbl TO '%s' (FORMAT PARQUET);", path))
  DBI::dbDisconnect(con, shutdown = TRUE)
  return(NULL)
}
