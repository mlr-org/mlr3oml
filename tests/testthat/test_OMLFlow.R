skip_on_cran()

test_that("Download mlr Flow: 3364", {
  with_public_server()
  flow_id = 3364L
  flow = OMLFlow$new(flow_id)
  expect_equal(flow$desc$upload_date, as.POSIXct("2016-03-21 16:06:43", tz = "UTC"))
  expect_equal(flow$description, "Learner classif.boosting from package(s) adabag, rpart.")
  expect_data_table(flow$parameter)
  expect_equal(names(flow$parameter), c("name", "data_type", "default_value"))
  expect_equal(flow$tag, "Verified_Supervised_Classification")
  expect_equal(flow$desc$version, 8L)
  expect_equal(flow$version_label, NULL)
  expect_equal(flow$dependencies, c("mlr_2.8", "adabag_4.1", "rpart_4.1.10"))
  expect_equal(flow$desc$uploader, 1160L)
  expect_equal(flow$id, 3364L)
  expect_equal(flow$name, "classif.boosting")
})

test_that("Cannot download non-existing flow", {
  invalid_id = 100000000L
  flow = OMLFlow$new(invalid_id)
  expect_error(flow$desc)
})

test_that("Autotest download", {
  with_public_server()
  n = 10
  ids = sample(load_ids("flow"), n)
  flows = mlr3misc::map(ids,
    function(x) {
      flow = OMLFlow$new(x)
      flow$desc
      return(flow)
    }
  )
  for (flow in flows) {
    expect_oml_flow(flow)
  }
})

test_that("Can reconstruct regr.rpart locally", {
  learner = mlr3::lrn("regr.rpart")
  pseudo_publish_learner(learner)
  flow = pseudo_get_flow("regr.rpart")
  expect_oml_flow(flow)
  expect_equal(learner, flow$convert())

})


skip("Don't randomly publish learners")
# TODO: check that this is working
test_that("Can reconstruct actual learner", {
  with_test_server()
  lrn = lrn("regr.rpart")
  response = publish(lrn)

  flow = OMLFlow$new(response)
  flow$desc
  expect_oml_flow(flow)
})
