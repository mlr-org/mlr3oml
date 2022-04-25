test_that("Can extract params from recursive components", {
  with_public_server()
  id = 5668
  flow = OMLFlow$new(id)
  psc = construct_paramset(flow)

  expected_ids = c(
    "steps_5668",
    "axis_5508",
    "copy_5508",
    "missing_values_5508",
    "strategy_5508",
    "verbose_5508",
    "categorical_features_5509",
    "dtype_5509",
    "handle_unknown_5509",
    "n_values_5509",
    "sparse_5509",
    "threshold_5669"
  )

  expect_equal(psc$ids(), expected_ids)
})
