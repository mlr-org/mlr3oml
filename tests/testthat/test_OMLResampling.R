test_that("OMLTask iris", {
  # TODO: This should be better tested with more resamplings
  with_public_server()
  oml_task = OMLTask$new(59)
  oml_resampling = oml_task$resampling
  expect_r6(oml_resampling, "OMLResampling")
  expect_integer(oml_resampling$id)
  expect_list(oml_resampling$estimation_procedure)
  expect_r6(oml_resampling$task, "OMLTask")
  resampling = oml_resampling$convert()
  expect_r6(resampling, "ResamplingCustom")
})
