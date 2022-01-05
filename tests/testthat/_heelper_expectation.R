#' This function takes a function that consists of expectations in each line,
#' wraps them around show_failure and does one expect that checks that all tests
#' passed. If it does not it prints the failure messages that were collected along
#' the way
make_expect = function(f) {
  body_list = as.list(body(f))
  # use <- insted of = because otherwise R thinks msgs is the argument
  body_list = mlr3misc::map(
    body_list[-1],
    function(x) {
      call("append", quote(msgs), call("show_failure", x))
    }
  )
  body_list = append(body_list, quote(expect(length(msgs) == 0,
    failure_message = paste0(msgs, collapse = "\n")
  )))
  body_list = append(body_list, quote(msgs <- list()), after = 0) # nolint

  body(f) = as.call(c(as.name("{"), body_list))
  return(f)
}

expect_oml_flow = make_expect(function(flow) {
  expect_r6(flow, "OMLFlow")
  expect_integer(flow$id)
  expect_posixct(flow$upload_date)
  expect_string(flow$description)
  expect_data_table(flow$parameter)
  expect_equal(
    names(flow$parameter),
    c("name", "data_type", "default_value", "description")
  )
  expect_character(flow$dependencies)
  expect_integer(flow$uploader)
  expect_integer(flow$id)
  expect_string(flow$name)
  expect_integer(flow$version)
})


expect_oml_data = function(oml_data) {
  expect_r6(oml_data, "OMLData")
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
  if (length(oml_data$target_names)) {
    expect_r6(oml_data$task(), "Task")
  }
}

expect_oml_task = function(oml_task) {
  expect_r6(oml_task, "OMLTask")
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
  expect_r6(oml_task$task, "Task")
  expect_r6(oml_task$resampling, "ResamplingCustom")

  expect_subset(unlist(oml_task$resampling$instance, use.names = FALSE), oml_task$task$row_ids)
}
