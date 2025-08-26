skip_on_cran()

test_that("list_oml_collections", {
  tab = list_oml_collections(main_entity_type = "run", status = "in_preparation", uploader = 1)
  expect_true(all(tab$status == "in_preparation"))
  expect_true(all(tab$creator == 1))
  expect_true(all(tab$main_entity_type == "run"))
})
