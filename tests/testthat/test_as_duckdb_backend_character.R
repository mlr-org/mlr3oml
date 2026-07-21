skip_if_not_installed("DBI")
skip_if_not_installed("duckdb")
skip_if_not_installed("mlr3db")

write_test_parquet = function(path) {
  con = DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con,
    "CREATE TABLE t AS SELECT i AS x, chr(CAST(65 + i % 3 AS INTEGER)) AS grp, i % 2 = 0 AS flag FROM range(1, 101) tbl(i)" # nolint
  )
  DBI::dbExecute(con, sprintf("COPY t TO '%s' (FORMAT PARQUET)", path))
}

test_that("backend reconnects after serialization", {
  path = tempfile(fileext = ".parquet")
  write_test_parquet(path)

  backend = as_duckdb_backend_character(path, factors = "grp")
  expect_backend(backend)

  # simulate shipping the backend to a parallel worker
  rds = tempfile(fileext = ".rds")
  saveRDS(backend, rds)
  restored = readRDS(rds)

  expect_false(restored$valid)
  tab = restored$data(rows = 1:5, cols = restored$colnames)
  expect_data_table(tab, nrow = 5)
  expect_factor(tab$grp)
  expect_equal(restored$nrow, 100L)
})
