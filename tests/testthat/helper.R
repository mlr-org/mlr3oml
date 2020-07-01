library(checkmate)
lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]$", full.names = TRUE), source)

expect_oml_data = function(oml_data) {
  expect_is(oml_data, "OMLData")
  expect_string(oml_data$name)
  expect_count(oml_data$nrow)
  expect_count(oml_data$ncol)
  expect_list(oml_data$desc, names = "unique")
  expect_data_table(oml_data$data, nrow = oml_data$nrow, ncol = oml_data$ncol)
  expect_character(oml_data$target_names, any.missing = FALSE, unique = TRUE)
  expect_character(oml_data$feature_names, any.missing = FALSE, unique = TRUE)
  expect_subset(oml_data$target_names, colnames(oml_data$data))
  expect_subset(oml_data$feature_names, colnames(oml_data$data))
  expect_disjunct(oml_data$target_names, oml_data$feature_names)
  if (length(oml_data$target_names))
    expect_is(oml_data$task(), "Task")
}

expect_oml_task = function(oml_task) {
  expect_is(oml_task, "OMLTask")
  expect_string(oml_task$name)
  expect_count(oml_task$nrow)
  expect_count(oml_task$ncol)
  expect_list(oml_task$desc, names = "unique")
  expect_data_table(oml_task$data$data, nrow = oml_task$nrow, ncol = oml_task$ncol)
  expect_character(oml_task$target_names, any.missing = FALSE, unique = TRUE)
  expect_character(oml_task$feature_names, any.missing = FALSE, unique = TRUE)
  expect_choice(oml_task$target_names, colnames(oml_task$data$data))
  expect_subset(oml_task$feature_names, colnames(oml_task$data$data))
  expect_disjunct(oml_task$target_names, oml_task$feature_names)
  expect_is(oml_task$task, "Task")
  expect_is(oml_task$resampling, "ResamplingCustom")

  expect_subset(unlist(oml_task$resampling$instance, use.names = FALSE), oml_task$task$row_ids)
}
