skip_on_cran()

# Test Info
# * Test for all 4 resamplings: CV, RepeatedCV, Holdout, LOO:
#   1. Check that expect_oml_data_split works
#   2. Manually verify that the converted splits are actually those expected
# * Test that there is a meaningful error when there are no dataplits
# * Multiple tests using OpenML-CC18
# * Test that the resampling's hash corresponds to the task's hash
# * Test that the oml_hash changes if the (instantiated) resampling is changed.
#   This is important when uploading runs, where we check for the oml_id but also have to check
#   that the resampling has not been changed since it was downloaded.

test_that("Basic test: iris", {
  with_public_server()
  oml_task = OMLTask$new(59)
  oml_data_split = oml_task$data_split
  expect_oml_data_split(oml_data_split)
})

test_that("Error when task does not provide task_splits", {
  expect_error(as_resampling(OMLTask$new(147517)),
    regexp = "Task not providing datasplits"
  )
})

test_that("Multiple tests", {
  set.seed(2)
  n = 5L
  task_ids = OMLCollection$new(99)$task_ids
  task_ids = sample(task_ids, size = n)
  for (task_id in task_ids) {
    oml_task = OMLTask$new(task_id)
    oml_data_split = oml_task$data_split
    expect_oml_data_split(oml_data_split)
  }
})

test_that("hash is correct", {
  otask = OMLTask$new(31)
  task = as_task(otask)
  resampling = as_resampling(otask)
  expect_true(task$hash == resampling$task_hash)
})


test_that("CV works", {
  id = 10107
  otask = OMLTask$new(id, cache = FALSE)
  task = as_task(otask)
  data_split = otask$data_split
  test_ids = data_split$splits["TEST", on = "type"][["rowid"]] + 1L
  resampling = as_resampling(data_split)
  expect_equal(test_ids, resampling$instance$row_id)
  expect_r6(resampling, "ResamplingCV")
})

test_that("CV repeated works", {
  id = 360928
  otask = OMLTask$new(id, cache = FALSE)
  task = as_task(otask)
  data_split = otask$data_split
  test_ids = data_split$splits["TEST", on = "type"][["rowid"]] + 1L
  resampling = as_resampling(data_split)
  expect_equal(test_ids, resampling$instance$row_id)
  expect_r6(resampling, "ResamplingRepeatedCV")
})

test_that("Leave One Out works", {
  id = 360927
  otask = OMLTask$new(id, cache = FALSE)
  task = as_task(otask)
  data_split = otask$data_split
  test_ids = data_split$splits["TEST", on = "type"][["rowid"]] + 1L
  resampling = as_resampling(data_split)
  expect_equal(test_ids, resampling$instance)
  expect_r6(resampling, "ResamplingLOO")
})

test_that("Holdout works", {
  id = 361001
  otask = OMLTask$new(id, cache = FALSE)
  task = as_task(otask)
  data_split = otask$data_split
  test_ids = data_split$splits["TEST", on = "type"][["rowid"]] + 1L
  train_ids = data_split$splits["TRAIN", on = "type"][["rowid"]] + 1L
  resampling = as_resampling(data_split)
  expect_equal(train_ids, resampling$instance$train)
  expect_equal(test_ids, resampling$instance$test)
  expect_r6(resampling, "ResamplingHoldout")
})
