skip_if(TRUE)
# always skip this, because the server resonse can be delayed and also because currently data
# cannot be uploaded to the test server

test_that("Can publish data", {
  with_public_server()
  withr::defer({delete("data", id, confirm = FALSE)})
  debugonce(upload)
  iris$id = 1:150
  name = "myiris"
  description = "my description..."
  row_identifier = "id"
  default_target = "Species"
  ignore = "Petal.Width"
  citation = "My citation"
  original_data_url = "www.mydata.de"
  paper_url = "www.mypaper.de"
  licence = "mylicence"

  id = publish(
    iris,
    name = name,
    desc = description,
    row_identifier = row_identifier,
    licence = licence,
    default_target = default_target,
    ignore = ignore,
    citation =  citation,
    original_data_url = original_data_url,
    paper_url = paper_url,
    confirm = FALSE
  )
  Sys.sleep(30)
  odata = OMLData$new(id)
  desc = odata$desc

  expect_true(desc$name == name)
  expect_true(desc$row_id_attribute == row_identifier)
  expect_true(desc$default_target_attribute == default_target)
  expect_true(desc$ignore_attribute == ignore)
  expect_true(desc$citation == citation)
  expect_true(desc$original_data_url == original_data_url)
  expect_true(desc$paper_url == paper_url)
  expect_true(desc$licence == licence)

  expect_identical(odata$features$name[odata$features$is_row_identifier], "id")
  expect_identical(odata$features$name[odata$features$is_ignore], "Petal.Width")
  expect_identical(odata$features$name[odata$features$is_target], "Species")
  expect_set_equal(colnames(odata$data), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Species"))
})
