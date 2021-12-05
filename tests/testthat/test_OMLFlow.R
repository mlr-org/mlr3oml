# skip_on_cran()

# Two kind of tests are done:
# 1. Automated random tests for minimal properties.
# 2. Specific tests on selected ids.


test_that("Download mlr Flow: 3364", {
  flow_id = 3364L
  flow_id = 8817
  flow = OMLFlow$new(flow_id)
  expect_equal(flow$upload_date, as.POSIXct("2016-03-21 16:06:43"))
  expect_equal(flow$description, "Learner classif.boosting from package(s) adabag, rpart.")
  expect_equal(flow$language, "English")
  expect_data_table(flow$parameter)
  expect_equal(names(flow$parameter), c("name", "data_type", "default_value", "description"))
  expect_equal(flow$tag, "Verified_Supervised_Classification")
  expect_equal(flow$version, 8L)
  expect_equal(flow$version_label, NULL)
  expect_equal(flow$dependencies, "mlr_2.8, adabag_4.1, rpart_4.1.10")
  expect_equal(flow$uploader, 1160L)
  expect_equal(flow$id, 3364L)
  expect_equal(flow$name, "classif.boosting")
})

test_that("Cannot download non-existing flow", {
  flow = OMLFlow$new(-100000000L)
  expect_error(flow$desc, silent = TRUE)
  expect_

})

test_that("Can construct pseudo learner", {
  ll = mlr3extralearners::list_mlr3learners(select = c("mlr3_package", "id", "required_packages",
                                                       "name", "class"))
  ll = ll[class %in% c("classif", "regr", "surv")]

  oml_list = list("oml_name" = sprintf("mlr3.%s", ll$id),
                  "dependencies" = ll$required_packages,
                  "class" = ll$class,
                  "name" = ll$name)

  for (i in seq_along(nrow(ll))) {
    # Create pseudoflow
    flow = list(name = oml_list$oml_name[[i]], dependencies = oml_list$dependencies[[i]])
    lrn = flow2mlr3(flow)
    expect_learner(lrn)
  }
})

test_that("Can construct actual learner", {
  options(mlr3oml.api_key = getOption("mlr3oml.test_api_key"),
          mlr3oml.server = getOption("mlr3oml.test_server"))
  with_test_server()
  lrn = lrn("regr.rpart")
  response = publish(lrn)

  flow = OMLFlow$new(response)
  flow$desc
  expect_character(flow$name)
})

test_that("Can construct GraphLearner", {

})

