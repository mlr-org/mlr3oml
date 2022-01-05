#' @title make_expect
#' @description Make a custom expectation function.
#' @details This function takes a function that consists of expectations in each line,
#' wraps them around show_failure and does one expect that checks that all tests
#' passed. If it does not it prints the failure messages that were collected along
#' the way. Because testthat and ceckmate expectations are mixed, we cannot rely on the
#' test_xxx function from checkmate but have to use expect + show_failure

make_expect = function(f, catch_id = TRUE) {
  body_list = as.list(body(f))[-1]
  # create testthat::show_failure(expect_xx(...))
  body_list = lapply(
    body_list,
    function(x) {
      ns_call = call("::", as.symbol("testthat"), as.symbol("show_failure"))
      as.call(list(ns_call, x))
    }
  )
  # create msgs <- append(testthat::show_failure(expect_xx(...)))
  body_list = lapply(
    body_list,
    function(x) call("<-", as.symbol("msgs"), call("append", as.symbol("msgs"), x))
  )

  # don't use = in quote because it is taken as the argument name
  body_list = append(body_list, quote(msgs <- list()), after = 0) # nolint
  if (catch_id) { # extract if of the tested object
    # when f = function(flow) ... obj becomes flow
    obj = as.symbol(formalArgs(eval(sys.call()[[2]]))[1])
    # extract the id from the oml object (e.g. flow)
    id_info = substitute(sprintf("id = %s\n", obj$id))
    failure_expr = call("paste0", id_info, quote(msgs), collapse = "\n")
  } else {
    failure_expr = call("paste0", quote(msgs), collapse = "\n")
  }
  body_list = append(body_list, call("<-", as.symbol("failure_message"), failure_expr))
  body_list = append(body_list, call("expect", quote(length(msgs) == 0),
    failure_message = quote(failure_message)
  ))
  body(f) = as.call(c(as.name("{"), body_list))
  return(f)
}

expect_oml_flow = make_expect(function(flow) {
  expect_r6(flow, "OMLFlow")
  expect_true(test_logical(flow$cache_dir) || test_character(flow$cache_dir))
  expect_integer(flow$id)
  expect_list(flow$desc)
  expect_character(flow$description)
  expect_data_table(flow$parameter)
  testthat::expect_equal(
    names(flow$parameter),
    c("name", "data_type", "default_value")
  )
  expect_character(flow$dependencies)
  expect_integer(flow$id)
  expect_string(flow$name)
  if (startsWith(flow$name, "mlr3.")) {
    expect_r6(flow$convert(), "Learner")
  } else {
    expect_true(is.null(flow$convert()))
  }
  expect_posixct(flow$desc$upload_date)
  expect_integer(flow$desc$version)
  expect_integer(flow$desc$uploader)
})

expect_oml_run = make_expect(function(run) {
  expect_r6(run, "OMLRun")
  expect_integer(run$id)
  expect_posixct(run$upload_date)
  expect_string(run$description)
  expect_data_table(run$prediction)
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
  expect_subset(
    unlist(oml_task$resampling$instance, use.names = FALSE), oml_task$task$row_ids
  )
}


# TODO :remoce the checkmate:: (because we import it) and import the relevant testthat expect files

expect_oml_run = make_expect(function(run) {
  expect_r6(run, "OMLRun")
  expect_integer(run$id)
  expect_posixct(run$upload_date)
  expect_string(run$description)
  expect_data_table(run$prediction)
})
