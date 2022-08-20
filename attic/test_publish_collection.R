skip_if(TRUE)

test_that("Can publish task collection", {
  with_test_server()
  withr::defer({delete("study", id, confirm = FALSE)})
  id = publish(c(1, 2), main_entity_type = "task", name = "test-study-oml", desc = "...", confirm = FALSE)
  ocol = OMLCollection$new(id)
  expect_true(ocol$main_entity_type == "task")
  expect_true(all(c(1, 2) %in% ocol$task_ids))
  expect_true(ocol$name == "test-study-oml")
  expect_true(ocol$desc$description == "...")
})

test_that("Can publish run collection", {
  with_test_server()
  withr::defer({delete("study", id, confirm = FALSE)})
  id = publish(c(1, 2), main_entity_type = "run", name = "test-study-oml", desc = "...", confirm = FALSE)
  ocol = OMLCollection$new(id)
  expect_true(ocol$main_entity_type == "run")
  expect_true(all(c(1, 2) %in% ocol$run_ids))
  expect_true(ocol$name == "test-study-oml")
  expect_true(ocol$desc$description == "...")
})
