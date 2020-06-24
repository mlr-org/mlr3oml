context("study_99")


test_that("study 99 can be loaded and parsed", {
  skip("Expensive manual test")

  data_ids = list_oml_data(tag = "study_99")$did

  for (data_id in data_ids) {
    expect_task(OMLData$new(data_id, use_cache = TRUE)$task)
  }
})
