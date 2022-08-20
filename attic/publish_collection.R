publish_collection = function(x, main_entity_type, name, desc, alias = NULL,
  api_key = get_api_key(), ...) {
  assert_choice(main_entity_type, c("task", "run"))
  assert_character(name, len = 1L, any.missing = FALSE)
  assert_character(desc, len = 1L, any.missing = FALSE)
  assert_character(alias, len = 1L, null.ok = TRUE)

  doc = xml2::xml_new_document()
  collection = xml2::xml_add_child(doc, "oml:study", "xmlns:oml" = "http://openml.org/openml")

  # Order matters!
  if (!is.null(alias)) {
    xml2::xml_add_child(.x = collection, .value = "oml:alias", alias)
  }
  xml2::xml_add_child(.x = collection, .value = "oml:main_entity_type", main_entity_type)
  xml2::xml_add_child(.x = collection, .value = "oml:name", name)
  xml2::xml_add_child(.x = collection, .value = "oml:description", desc)

  objects = xml2::xml_add_child(collection, .value = sprintf("oml:%ss", main_entity_type))
  for (id in x) {
    xml2::xml_add_child(.x = objects, .value = sprintf("oml:%s_id", main_entity_type), id)
  }

  desc_path = tempfile(fileext = ".xml")
  withr::defer(unlink(desc_path))
  xml2::write_xml(x = doc, file = desc_path)

  upload(
    url = sprintf("%s/study", get_server()),
    body = list(
      description = httr::upload_file(desc_path)
    ),
    query = list(api_key = api_key)
  )
}
