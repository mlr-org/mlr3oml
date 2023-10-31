skip()

test_that("data publishing works", {
  test_server = TRUE
  data = iris
  withr::defer(delete(type = "data", id = id, test_server = test_server))
  data$id = 1:150
  name = "a"
  desc = "b"
  license = "c"
  default_target = "Species"
  ignore_attribute = "Sepal.Length"
  row_identifier = "id"
  citation = "d"
  original_data_url = "e"
  paper_url = "f"


  id = publish_data(
    data = data,
    name = name,
    desc = desc,
    license = license,
    default_target = default_target,
    citation = citation,
    row_identifier = row_identifier,
    ignore_attribute = ignore_attribute,
    original_data_url = original_data_url,
    paper_url = paper_url,
    test_server = test_server
  )

  expect_int(id)
  Sys.sleep(10)

  odata = odt(id, test_server = test_server)
  expect_oml_data(odata)

  expect_set_equal(colnames(odata$data), setdiff(colnames(iris), ignore_attribute))
  expect_equal(odata$desc$licence, license)
  expect_equal(odata$desc$citation, citation)
  expect_equal(odata$desc$row_id_attribute, row_identifier)
  expect_equal(odata$desc$ignore_attribute, ignore_attribute)
  expect_equal(odata$desc$original_data_url, original_data_url)
  expect_equal(odata$desc$paper_url, paper_url)
})
