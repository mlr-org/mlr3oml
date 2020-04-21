expect_oml_data = function(oml_data) {
  expect_string(x$name)
  expect_count(x$nrow)
  expect_count(x$ncol)
  expect_list(description, names = "unique")
  expect_data_table(x$data, nrow = x$nrow, ncol = x$ncol)
  expect_choice(x$target_names, colnames(x$data))
  expect_is(x$task, "Task")
}
