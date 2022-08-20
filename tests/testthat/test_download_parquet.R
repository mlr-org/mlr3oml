skip_on_cran()

skip_if_not_installed("mlr3db")
skip_if_not_installed("DBI")
skip_if_not_installed("duckdb")

test_that("make_names_parquet works", {
  require_namespaces(c("duckdb", "DBI"))
  odata = OMLData$new(9, parquet = FALSE, cache = FALSE)
  path = odata$desc$parquet_url

  path = tempfile(fileext = ".parquet")
  download.file(odata$desc$parquet_url, path)

  dat1 = read_parquet(path)

  con = DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, sprintf("CREATE TABLE tbl AS SELECT * FROM '%s';", path))
  names = DBI::dbGetQuery(con, "PRAGMA table_info('tbl')")$name
  DBI::dbDisconnect(con, shutdown = TRUE)

  names_old = names(odata$data)
  normalize_names_parquet_inplace(path)
  dat2 = read_parquet(path)

  for (i in seq_along(dat1)) {
    expect_equal(dat1[[i]], dat2[[i]])
  }

  backend = mlr3db::as_duckdb_backend(path)
  expect_true("mlr3_row_id" %in% backend$colnames)
  expect_set_equal(setdiff(backend$colnames, "mlr3_row_id"), make.names(names_old))
})
