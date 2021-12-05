#skip_on_cran()

test_that("OMLCollection CC-18", {
  id = 276
  c = OMLCollection$new(id)
  tasks = c$tasks$convert()
  expect_equal(c$name, "CC18-Example")
  expect_equal(c$creator, 869L)
  # has no creation_date
  # has no alias
  # has no tag
  expect_equal(c$main_entity_type, "run")
  expect_equal(c$description, "Description")
  expect_equal(c$flow_ids, 18995L)
  expect_equal(c$data_ids, c(1063L, 1590))
  expect_equal(c$run_ids, c(10560782L, 10560783L))
  expect_equal(c$task_ids, c(3913L, 7592L))
  expect_r6(c$runs, "OMLContainer")
  expect_r6(c$flows, "OMLContainer")
  expect_r6(c$data, "OMLContainer")
  expect_r6(c$tasks, "OMLContainer")
  expect_true(c$runs$contains == "OMLRun")
  expect_true(c$flows$contains == "OMLFlow")
  expect_true(c$data$contains == "OMLData")
  expect_true(c$tasks$contains == "OMLTask")
})
id = 15
c = OMLCollection$new(id)
c$flows$get(1)
