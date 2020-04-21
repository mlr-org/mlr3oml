expect_oml_data = function(oml_data) {
  expect_string(oml_data$name)
  expect_count(oml_data$nrow)
  expect_count(oml_data$ncol)
  expect_list(oml_data$description, names = "unique")
  expect_data_table(oml_data$data, nrow = oml_data$nrow, ncol = oml_data$ncol)
  expect_choice(oml_data$target_names, colnames(oml_data$data))
  expect_is(oml_data$task, "Task")
}
