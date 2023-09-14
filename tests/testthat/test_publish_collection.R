skip_on_cran()

test_that("Can publish collection on the test server", {
  test_server = TRUE
  withr::defer(delete(type = "collection", id = collection_id, test_server = test_server))

  collection_id = publish_collection(
    ids = c(1, 2),
    main_entity_type = "task",
    name = "test",
    desc = "test collection",
    alias = "test_alias",
    test_server = test_server
  )

  assert_int(collection_id)
  Sys.sleep(5)

  ocollection = ocl(collection_id, test_server = test_server)
  expect_oml_collection(ocollection)

  expect_equal(ocollection$main_entity_type, "task")
  expect_set_equal(ocollection$task_ids, c(1, 2))
  expect_equal(ocollection$desc$description, "test collection")
  expect_equal(ocollection$desc$name, "test")
})
