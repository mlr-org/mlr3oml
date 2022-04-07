test_that("Can extract params from recursive components", {
  with_public_server()
  id = 5668
  flow = OMLFlow$new(id)
  psc = construct_paramset(flow)
  expected_ids = c(
    "steps",
    "c5508.axis",
    "c5508.copy",
    "c5508.missing_values",
    "c5508.strategy",
    "c5508.verbose",
    "c5509.categorical_features",
    "c5509.dtype",
    "c5509.handle_unknown",
    "c5509.n_values",
    "c5509.sparse",
    "c5669.threshold"
  )
  expect_equal(psc$ids(), expected_ids)
})
