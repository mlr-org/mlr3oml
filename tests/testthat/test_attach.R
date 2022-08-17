test_that("attach and detach work", {
  with_test_server()
  ocol = OMLCollection$new(4L, FALSE)
  assert_true(length(ocol$task_ids) == 1L)

  attach_to_study(4, 5)
  ocol = OMLCollection$new(4L, FALSE)
  expect_true(length(ocol$task_ids) == 2L)

  detach_from_study(4, 5)
  ocol = OMLCollection$new(4L, FALSE)
  expect_true(length(ocol$task_ids) == 1L)
})
