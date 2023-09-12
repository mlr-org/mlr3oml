skip("OpenML Test server is unstable")

test_that("Can publish collection on public server", {
  test_server = FALSE
  withr::defer(delete(type = "collection", id = collection_id, test_server = test_server))

  collection_id = publish_collection(
    ids = c(31, 61),
    main_entity_type = "task",
    name = "test",
    desc = "test collection",
    alias = "test_alias",
    test_server = test_server
  )

  assert_int(collection_id)
  Sys.sleep(5)

  ocollection = ocl(collection_id)
  expect_oml_collection(ocollection)

  expect_equal(ocollection$main_entity_type, "task")
  expect_set_equal(ocollection$task_ids, c(31, 61))
  expect_equal(ocollection$desc$description, "test collection")
  expect_equal(ocollection$desc$name, "test")

})
