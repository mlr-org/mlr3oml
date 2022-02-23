test_that("Can convert tasks, flows, data and runs to data.tables", {
  with_public_server()
  id = 232
  c = OMLCollection$new(id)
  tasks_dt = as.data.table(c$tasks)
  flows_dt = as.data.table(c$flows)
  data_dt = as.data.table(c$dat)
  runs_dt = as.data.table(c$runs)
  expect_data_table(tasks_dt, nrows = 2L)
  expect_data_table(flows_dt, nrows = 2L)
  expect_data_table(data_dt, nrows = 2L)
  expect_data_table(runs_dt, nrows = 2L)

  expect_equal(c$name, "Test-Study")
  expect_equal(c$creator, 869L)
})

test_that("Can extract tasks, flows, data and runs from Dictionary", {
  id = 232
  c = OMLCollection$new(id)
  tasks_dt = as.data.table(c$tasks)
  flows_dt = as.data.table(c$flows)
  data_dt = as.data.table(c$dat)
  runs_dt = as.data.table(c$runs)
  expect_data_table(tasks_dt)
  expect_data_table(flows_dt, nrows = 2L)
  expect_data_table(data_dt)
  expect_data_table(runs_dt)

  expect_equal(c$name, "CC18-Example")
  expect_equal(c$creator, 869L)
})
