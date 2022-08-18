skip_on_cran()

test_that("Can properly parse the ignore_attribute", {
  desc_basic = list(
    format = "arff",
    id = 1,
    version = 1,
    default_target_attribute = "Species",
    upload_date = as.Date("2022-04-25"),
    processing_data = as.Date("2022-04-25")
  )
  desc1 = desc_basic
  desc1$ignore_attribute = c("a,b", "c")
  desc1_parsed = parse_desc_data(desc1)
  expect_true(setequal(desc1_parsed$ignore_attribute, c("a", "b", "c")))

  desc2 = desc_basic
  desc2$ignore_attribute = c("a,b", "c,d")
  desc2_parsed = parse_desc_data(desc2)
  expect_true(setequal(desc2_parsed$ignore_attribute, c("a", "b", "c", "d")))

  desc3 = desc_basic
  desc3$ignore_attribute = c("a", "b")
  desc3_parsed = parse_desc_data(desc3)
  expect_true(setequal(desc3_parsed$ignore_attribute, c("a", "b")))
})
