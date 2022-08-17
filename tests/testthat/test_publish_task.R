skip_if(TRUE)

test_that("Can publish task", {
  with_public_server()
  withr::defer({delete("task", id1, confirm = FALSE)})
  expect_message(
    id1 <<- publish(
      "task",
      data_id = 61,
      type = "regr",
      target = "sepallength",
      confirm = FALSE,
      estimation_procedure = 8
    ),
    regexp = NA
  )
  expect_integer(id1)
  expect_message(
    id1 <<- publish(
      "task",
      data_id = 61,
      type = "regr",
      target = "sepallength",
      confirm = FALSE,
      estimation_procedure = 8
    ),
    regexp = "Task already exists with id",
    fixed = TRUE
  )
  expect_true(id1 == id2)
  expect_warning(
    response <<- publish(
      "task",
      data_id = 61,
      type = "regr",
      target = "abcde",
      confirm = FALSE,
      estimation_procedure = 8
    ),
    regexp = "Input value does not match allowed values in foreign column",
    fixed = TRUE
  )
})
