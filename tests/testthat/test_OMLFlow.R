skip_on_cran()

test_that("Download mlr Flow: 3364", {
  with_public_server()
  flow_id = 3364L
  flow = OMLFlow$new(flow_id, FALSE)
  expect_equal(flow$desc$upload_date, as.POSIXct("2016-03-21 16:06:43", tz = "UTC"))
  expect_data_table(flow$parameters)
  expect_equal(names(flow$parameters), c("name", "data_type", "default_value"))
  expect_equal(flow$tags, "Verified_Supervised_Classification")
  expect_equal(flow$desc$version, 8L)
  expect_equal(flow$dependencies, "mlr_2.8, adabag_4.1, rpart_4.1.10")
  expect_equal(flow$desc$uploader, 1160L)
  expect_equal(flow$id, 3364L)
})

test_that("Cannot download non-existing flow", {
  with_public_server()
  invalid_id = 100000000L
  flow = OMLFlow$new(invalid_id, FALSE)
  expect_error(flow$desc)
})

test_that("Flows 17374 17369", {
  with_public_server()
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
  with_public_server()
  oml_flow = OMLFlow$new(1, F)
  learner_classif = as_learner(oml_flow, "classif")
  learner_regr = as_learner(oml_flow, "regr")
  expect_warning(learner_null <<- as_learner(oml_flow))
  expect_r6(learner_classif, "LearnerClassifOML1")
  expect_r6(learner_regr, "LearnerRegrOML1")
  expect_true(is.null(learner_null))
})


test_that("Conversion of mlr flow works as intended", {
  id = 19052L
  flow = OMLFlow$new(id, FALSE)
  learner = as_learner(flow, "classif")
  expect_r6(learner, "LearnerClassifOML19052")
})

test_that("Test publish, download convert, publish lifecycle", {
  with_test_server()
  # TODO:
  # 1. publish flow
  # 2. Download flow
  # 3. convert flow using as_learner and compare
  # 4. Upload again and check that same id is returned
  # 5. delete flow
})

test_that("Correct warning message regarding dependencies.", {
  flow = OMLFlow$new(19082)
  flow$desc # need to touch it so that the desc is loaded

  # flow$.__enclos_env__$private$.desc$dependencies = c("aaa_1")
  pkg_correct = paste0(paste(installed.packages()["utils", c("Package", "Built")], collapse = "_"))
  pkg_wrong_version = paste0(paste(installed.packages()["utils", c("Package", "Built")], collapse = "_"), ".3")
  R_version_running = paste0("R_", paste0(R.Version()[c("major", "minor")], collapse = "."))


  # Correct package but version conflict
  flow$.__enclos_env__$private$.desc$dependencies = pkg_wrong_version
  expect_message(as_learner(flow, verbose = TRUE), regexp = "Version conflicts")

  # missing dependencies + a version conflict
  flow$.__enclos_env__$private$.desc$dependencies = c(pkg_wrong_version, "aa_1")
  expect_message(as_learner(flow, verbose = TRUE), regexp = "has uninstalled dependencies")
  expect_message(as_learner(flow, verbose = TRUE), regexp = "Version conflicts")


  # wrong R version and missing all required dependencies
  flow$.__enclos_env__$private$.desc$dependencies = c("R_1.1", ".abcdefg_1")
  expect_message(as_learner(flow, verbose = TRUE), regexp = "Flow's R version")
  expect_message(as_learner(flow, verbose = TRUE), regexp = "Missing all required dependencies")

  # everything as expected
  flow$.__enclos_env__$private$.desc$dependencies = c(
    R_version_running, pkg_correct
  )
  expect_message(as_learner(flow, verbose = TRUE), regexp = NA)
})
