skip_on_cran()

test_that("OMLTask iris", {
  with_public_server()
  oml_task = OMLTask$new(59)
  oml_data_split = oml_task$data_split
  expect_oml_data_split(oml_data_split)
})

test_that("Error when task does not provide task_splits", {
  expect_error(as_resampling(OMLTask$new(147517)))
})

test_that("Multiple tests", {
  task_ids = c(1, 2, 3, 4)
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
  resampling1 = rsmp("cv", folds = 10)$instantiate(task)
  resampling2 = as_resampling(data_split)
  # 3 mismatches: super$hash, hash and the instance
  expect_true(length(all.equal(resampling1, resampling2)) == 4L)
  expect_true(nrow(resampling1$instance) == nrow(resampling2$instance))
})

test_that("CV repeated works", {
  id = 360928
  otask = OMLTask$new(id, cache = FALSE)
  task = as_task(otask)
  data_split = otask$data_split
  resampling1 = rsmp("repeated_cv", folds = 2, repeats = 5)$instantiate(task)
  resampling2 = as_resampling(data_split)
  # 3 mismatches: super$hash, hash and the instance
  expect_true(length(all.equal(resampling1, resampling2)) == 4L)
  expect_true(nrow(resampling1$instance) == nrow(resampling2$instance))
  expect_set_equal(resampling1$instance$rep, resampling2$instance$rep)
  expect_set_equal(resampling1$instance$fold, resampling2$instance$fold)
  expect_set_equal(resampling1$instance$row_id, resampling2$instance$row_id)
})

test_that("Leave One Out works", {
  id = 360927
  otask = OMLTask$new(id, cache = FALSE)
  task = as_task(otask)
  data_split = otask$data_split
  resampling1 = rsmp("loo")$instantiate(task)
  resampling2 = as_resampling(data_split)
  # 3 mismatches: super$hash, hash and the instance
  expect_true(length(all.equal(resampling1, resampling2)) == 4L)
  expect_true(length(resampling1$instance) == length(resampling2$instance))
})

test_that("Holdout works", {
  id = 361001
  otask = OMLTask$new(id, cache = FALSE)
  task = as_task(otask)
  data_split = otask$data_split
  resampling2 = as_resampling(data_split)
  resampling1 = rsmp("holdout", ratio = resampling2$param_set$values$ratio)$instantiate(task)
  # 3 mismatches: super$hash, hash and the instance
  expect_true(length(all.equal(resampling1, resampling2)) == 5L)
  expect_true(length(resampling1$instance) == length(resampling2$instance))
  expect_true(length(resampling1$instance$train) == length(resampling2$instance$train))
  expect_true(length(resampling1$instance$test) == length(resampling2$instance$test))
})

test_that("oml_hash changes when task is modified and get_oml_id returns NULL", {
  otask = OMLTask$new(31)
  resampling = as_resampling(otask)
  expect_warning(get_oml_id_resampling(resampling), NA)
  resampling$param_set$values$folds = 2L
  expect_warning(
    get_oml_id_resampling(resampling),
    "This resampling was constructed from an OpenML task split but was modified."
  )
})
