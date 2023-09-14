#' @title Publish a Collection to OpenML
#'
#' @description
#' Publish a collection to OpenML
#' This can also be achieved through the [website](https://openml.org).
#'
#' @param ids (`integer()`)\cr
#'   The IDs to include in the collection.
#'   Depending on the main entity tupe, these can be task or run IDs.
#' @param main_entity_type (`character(1)`)\cr
#'   The main entity type of the collection. Can be either "task" or "run".
#' @param name (`character(1)`)\cr
#'   The name for the collection.
#' @param desc (`character(1)`)\cr
#'   The description of the collection.
#' @param alias (`character(1)`)\cr
#'   The alias for the collection.
#' @template param_test_server
#' @template param_api_key
#'
#' @export
publish_collection = function(ids, name, desc, main_entity_type = "task", alias = NULL, api_key = NULL,
  test_server = test_server_default()) {
  require_namespaces(c("xml2", "httr"))
  assert_flag(test_server)
  if (is.null(api_key)) {
    api_key = get_api_key(get_server(test_server))
  } else {
    assert_string(api_key)
  }
  assert_choice(main_entity_type, c("task", "run"))
  assert_string(name)
  assert_string(desc)
  assert_string(alias, null.ok = TRUE)

  doc = xml2::xml_new_document()
  collection = xml2::xml_add_child(doc, "oml:study", "xmlns:oml" = "http://openml.org/openml")

  # Order matters!
  if (!is.null(alias)) xml2::xml_add_child(.x = collection, .value = "oml:alias", alias)
  xml2::xml_add_child(.x = collection, .value = "oml:main_entity_type", main_entity_type)
  xml2::xml_add_child(.x = collection, .value = "oml:name", name)
  xml2::xml_add_child(.x = collection, .value = "oml:description", desc)

  objects = xml2::xml_add_child(collection, .value = sprintf("oml:%ss", main_entity_type))
  for (id in ids) {
    xml2::xml_add_child(.x = objects, .value = sprintf("oml:%s_id", main_entity_type), id)
  }

  desc_path = tempfile(fileext = ".xml")
  withr::defer(unlink(desc_path))
  xml2::write_xml(x = doc, file = desc_path)

  response = httr::POST(
    url = sprintf("%s/study", get_server(test_server)),
    body = list(
      description = httr::upload_file(desc_path)
    ),
    query = list(api_key = api_key)
  )


  response_list = xml2::as_list(httr::content(response))
  if (httr::http_error(response)) {
    warningf(
      paste(response_list$error$message, response_list$error$additional_information, collapse = "\n")
    )
    return(response)
  }
  as.integer(response_list$study_upload$id[[1L]])
}
