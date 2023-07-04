skip_on_cran()

test_that("Download mlr Flow: 3364", {
  local_public_server()
  flow_id = 3364L
  flow = OMLFlow$new(flow_id, FALSE)
  expect_equal(flow$desc$upload_date, as.POSIXct("2016-03-21 16:06:43", tz = "UTC"))
  expect_data_table(flow$parameter)
  expect_equal(names(flow$parameter), c("name", "data_type", "default_value"))
  expect_equal(flow$tags, "Verified_Supervised_Classification")
  expect_equal(flow$desc$version, 8L)
  expect_equal(flow$dependencies, "mlr_2.8, adabag_4.1, rpart_4.1.10")
  expect_equal(flow$desc$uploader, 1160L)
  expect_equal(flow$id, 3364L)
})

test_that("Cannot download non-existing flow", {
  local_public_server()
  invalid_id = 100000000L
  flow = OMLFlow$new(invalid_id, FALSE)
  expect_error(flow$desc)
})

test_that("Flows 17374 17369", {
  local_public_server()
  ids = OMLCollection$new(232, FALSE)$flow_ids

  flows = mlr3misc::map(
    ids,
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

test_that("Can convert pseudo OML Learner", {
  local_public_server()
  oml_flow = OMLFlow$new(1, F)
  learner_classif = mlr3::as_learner(oml_flow, "classif")
  learner_regr = mlr3::as_learner(oml_flow, "regr")
  expect_r6(learner_classif, "LearnerClassifOML1")
  expect_r6(learner_regr, "LearnerRegrOML1")
})


test_that("Conversion of mlr flow works as intended", {
  id = 19052L
  flow = OMLFlow$new(id, FALSE)
  learner = mlr3::as_learner(flow, "classif")
  expect_r6(learner, "LearnerClassifOML19052")
})

test_that("Can open help page for OpenML Flow", {
  expect_error(OMLFlow$new(1)$help(), regexp = NA)
})

test_that("Printer works", {
  local_log_info()

  with_cache({
    oml_flow = oflw(6794)
    observed = capture.output(print(oml_flow))[2:4]
    expected = c(
      "<OMLFlow:6794>",
      " * Name: mlr.classif.ranger",
      " * Dependencies: R_3.3.2, OpenML_1.3, mlr_2.11, ranger_0.8.0"
    )
    expect_equal(observed, expected)
    }, cache = FALSE)

})


test_that("download runs without error", {
  local_log_info()

  # simple sanity check
  out = capture.output(with_cache(oflw(6794)$download(), cache = FALSE))
  expect_true(length(out) == 1L)
})
