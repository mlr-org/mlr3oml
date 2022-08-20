skip_if(TRUE)

test_that("Can publish learner", {
  with_public_server()
  withr::defer({delete("flow", id1, confirm = FALSE)})
  learner = lrn("regr.rpart")
  tmp = paste(sample(letters, 20), collapse = "")
  # just to make sure that this is not already published. T
  # This changes the hash of the learner which is used as the external version
  class(learner) = c(class(learner), tmp)
  class
  id1 = publish(learner, confirm = FALSE)

  oflow = OMLFlow$new(id1)
  expect_oml_flow(oflow)

  learner$param_set$values$xval = 5L
  expect_message(
    id2 <<- publish(learner, confirm = FALSE),
    regexp = sprintf("Flow already exists with id %s.", id1),
    fixed = TRUE
  )
  expect_true(id2 == id1)

})


test_that("Can publish learner with list parameter", {
  with_public_server()
  withr::defer(delete("flow", id, confirm = FALSE))
  learner = lrn("classif.rpart")
  class(learner) = c(class(learner), paste0(sample(letters, 20), collapse = ""))
  learner$.__enclos_env__$private$.param_set$add(paradox::ParamUty$new(id = "unused1", default = na.omit))
  id = publish(learner, confirm = FALSE)
  oflow = OMLFlow$new(id)

  # NoDefault works
  observed = oflow$parameters$default_value[oflow$parameters$name == "minbucket"]
  expected = "<NoDefault>"
  expect_true(observed == expected)

  # weird parameters work
  observed = oflow$parameters$default_value[oflow$parameters$name == "unused1"]
  expected = jsonlite::toJSON(na.omit)
  expect_true(observed[1] == expected[1] && observed[2] == expected[2])

  # normal parameters are being parsed correctly
  observed = oflow$parameters$default_value[oflow$parameters$name == "cp"]
  expected = 0.01
  expect_true(observed == expected)
})
