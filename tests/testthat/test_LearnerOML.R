skip_on_cran()

test_that("Can extract params from recursive components", {
  with_public_server()
  id = 5668
  flow = OMLFlow$new(id)
  psc = construct_paramset(flow)

  expected_ids = c(
    "f5668.steps",
    "f5508.axis",
    "f5508.copy",
    "f5508.missing_values",
    "f5508.strategy",
    "f5508.verbose",
    "f5509.categorical_features",
    "f5509.dtype",
    "f5509.handle_unknown",
    "f5509.n_values",
    "f5509.sparse",
    "f5669.threshold"
  )

  expect_equal(psc$ids(), expected_ids)
})
