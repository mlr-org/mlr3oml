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
      catf(" * Estimation: holdout (id: %s; test size: %s%%)",
        estimation_procedure$id,
        parameter[get("name") == "percentage", "value"][[1L]]
      )
    } else if (type == "leaveoneout") {
      catf(" * Estimation: leaveoneout (id: %s)", estimation_procedure$id)
    }
  } else {
    catf(" * Estimation: <missing>")
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

# remove this when it is merged in mlr3db (... in mlr3db is not passed to duckdb constructor...)
as_duckdb_backend_character = function(data, primary_key = NULL, factors) {
  require_namespaces(c("DBI", "duckdb", "mlr3db"))

  assert_file_exists(data, access = "r", extension = "parquet")
  con = DBI::dbConnect(duckdb::duckdb())
  on.exit({DBI::dbDisconnect(con, shutdown = TRUE)}, add = TRUE)

  # 1. view: we create the data as is
  tbl = "mlr3db_view"
  query = sprintf("CREATE OR REPLACE VIEW '%s' AS SELECT *", tbl)
  if (is.null(primary_key)) {
    primary_key = "mlr3_row_id"
    query = paste0(query, ", row_number() OVER () AS mlr3_row_id")
  } else {
    assert_string(primary_key)
  }

  query = sprintf("%s FROM parquet_scan(['%s'])", query, paste0(data, collapse = "','"))
  DBI::dbExecute(con, query)

  # 2. view: we encode the booleans as VARCHAR
  table_info = DBI::dbGetQuery(con, sprintf("PRAGMA table_info('%s')", tbl))
  vars_orig = table_info$name
  if (any(table_info$type == "BOOLEAN")) {
    tbl_prev = tbl
    tbl = "mlr3db_view_recoded"
    type = table_info$type
    vars = paste0("\"", vars_orig, "\"")
    tmp = vars[type == "BOOLEAN"]
    vars[type == "BOOLEAN"] = paste0(tmp, "::VARCHAR as", tmp)
    vars = paste(vars, collapse = ", ")

    query = sprintf("CREATE OR REPLACE VIEW '%s' AS SELECT %s from '%s'", tbl, vars, tbl_prev)
    DBI::dbExecute(con, query)
  }

  # 3. view: we normalize the names
  table_info = DBI::dbGetQuery(con, sprintf("PRAGMA table_info('%s')", tbl))
  old = table_info$name
  new = make.names(vars_orig)
  if (anyDuplicated(new)) {
    # This should be caught much earlier already (when downloading the data description)
    # but we add it here as a precaution
    stopf("Duplicated names detected after conversion.")
  }
  # recoding bools always creates names that have to be fixed
  if (any(old != new)) {
    tbl_prev = tbl
    tbl = "mlr3db_view_renamed"
    old = table_info$name
    new = make.names(vars_orig)

    if (anyDuplicated(new)) {
      # this might be triggered by a duckdb bug
      # https://github.com/duckdb/duckdb/issues/4806
      stopf("No unique names after conversion.")
    }

    tmp_old = paste0("\"", old, "\"")
    tmp_new = paste0("\"", new, "\"")
    renamings = paste(tmp_old, "AS", tmp_new, collapse = ", ")

    query = sprintf("CREATE OR REPLACE VIEW '%s' AS SELECT %s from '%s'",
      tbl, renamings, tbl_prev
    )
    DBI::dbExecute(con, query)
  }

  backend = mlr3db::DataBackendDuckDB$new(con, table = tbl, primary_key = primary_key,
    strings_as_factors = factors
  )

  on.exit()

  backend
}



#' @title Delete an object from OpenML
#' @description This function can be used to delete objects from the OpenML server.
#'
#' @param type (`character(1)`)
#'  What type of object should be deleted ("flow", "run", "task", "data", "study" / "collection")
#' @param id (`integer(1)`)
#'  The id of the object that will be deleted.
#' @param api_key (`character(1)`)
#'  The API key to perform the action, if left NULL it first tries getOption("mlr3oml.api_key") and
#'  then Sys.getenv("OMLAPIKEY").
#' @param server (`character(1)`)
#'  The server address, defaults to `FALSE`.
#' @param confirm (`logical(1)`)
#'  Whether the deletion has to be confirmed interactively.
#' @noRd
delete = function(type, id, api_key = NULL, test_server = FALSE) {
  server = get_server(assert_flag(test_server))
  if (is.null(api_key)) api_key = get_api_key(server)
  assert_choice(type, choices = c("flow", "run", "task", "data", "study", "collection"))

  if (type == "collection") type = "study"
  url = sprintf("%s/%s/%s", server, type, id)
  response = httr::DELETE(url, query = list(api_key = api_key))
  response
}
