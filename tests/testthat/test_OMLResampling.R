skip_on_cran()
test_that("OMLTask iris", {
  # TODO: This should be better tested with more resamplings
  with_public_server()
  oml_task = OMLTask$new(59)
  oml_resampling = oml_task$resampling
  expect_oml_resampling(oml_resampling)
})

test_that("Error when task does not provide task_splits", {
  expect_error(OMLTask$new(147517)$resampling$convert())
})

test_that("Multiple tests", {
  task_ids = c(1, 2, 3, 4)
  for (task_id in task_ids) {
    oml_task = OMLTask$new(task_id)
    oml_resampling = oml_task$resampling
    expect_oml_resampling(oml_resampling)
  }
})
