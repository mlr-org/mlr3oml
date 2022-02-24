# skip_on_cran()

test_that("Download mlr Flow: 3364", {
  with_public_server()
  flow_id = 3364L
  flow = OMLFlow$new(flow_id)
  expect_equal(flow$desc$upload_date, as.POSIXct("2016-03-21 16:06:43", tz = "UTC"))
  expect_data_table(flow$parameter)
  expect_equal(names(flow$parameter), c("name", "data_type", "default_value"))
  expect_equal(flow$tags, "Verified_Supervised_Classification")
  expect_equal(flow$desc$version, 8L)
  expect_equal(flow$version_label, NULL)
  expect_equal(flow$dependencies, c("mlr_2.8", "adabag_4.1", "rpart_4.1.10"))
  expect_equal(flow$desc$uploader, 1160L)
  expect_equal(flow$id, 3364L)
})

test_that("Cannot download non-existing flow", {
  with_public_server()
  invalid_id = 100000000L
  flow = OMLFlow$new(invalid_id)
  expect_error(flow$desc)
})

test_that("Autotest download", {
  with_public_server()
  n = 2
  ids = sample(load_ids("flow"), n)
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
  with_public_server()
  oml_flow = OMLFlow$new(1)
  learner_classif = oml_flow$convert("classif")
  learner_regr = oml_flow$convert("regr")
  learner_null = oml_flow$convert()
  expect_r6(learner_classif, "LearnerClassifOML1")
  expect_r6(learner_regr, "LearnerRegrOML1")
  expect_true(is.null(learner_null))
})

# Requiring download
test_that("Uploading flow works correctly", {
  # We test that:
  # 1. We can uppload a flow.
  # 2. Uploading it again returns the 2. id.
  # 3. We can convert it and it is identical to the learner
  with_test_server()
  withr::defer(delete("flow", flow_id, confirm = FALSE))
  learner = mlr3::lrn("regr.rpart")
  flow_id = publish(learner, confirm = FALSE)
  flow_id_second = publish(learner, confirm = FALSE)
  flow = OMLFlow$new(flow_id)
  expect_equal(flow_id, flow_id_second)
  expect_equal(flow$convert(), learner)
})

test_that("Conversion of mlr flow works as intended", {
  id = 19052L
  flow = OMLFlow$new(id)
  learner = flow$convert("classif")
  expect_r6(learner, "LearnerClassifOML19052")
})
