#' @export
publish.data.frame = function(x, name, desc, licence = "Public", default_target = NULL,
  row_identifier = NULL, ignore = NULL, citation = NULL, original_data_url = NULL,
  paper_url = NULL, api_key = get_api_key(), ...) {
  assert_character(name, len = 1, min.chars = 1L, null.ok = TRUE)
  assert_character(desc, len = 1, null.ok = TRUE)
  assert_character(licence, len = 1, null.ok = TRUE)
  assert_choice(default_target, colnames(x), null.ok = TRUE)
  assert_choice(row_identifier, colnames(x), null.ok = TRUE)
  assert_choice(ignore, colnames(x), null.ok = TRUE)
  assert_character(citation, len = 1L, null.ok = TRUE)
  assert_character(original_data_url, len = 1L, null.ok = TRUE)
  assert_character(paper_url, len = 1L, null.ok = TRUE)

  doc = xml2::xml_new_document()
  dat = xml2::xml_add_child(doc, "oml:data_set_description", "xmlns:oml" = "http://openml.org/openml")

  # Order matters!
  xml2::xml_add_child(.x = dat, .value = "oml:name", name)
  xml2::xml_add_child(.x = dat, .value = "oml:description", desc)
  xml2::xml_add_child(.x = dat, .value = "oml:format", "arff")
  if (!is.null(license)) {
    xml2::xml_add_child(.x = dat, .value = "oml:licence", licence)
  }
  if (!is.null(default_target)) {
    xml2::xml_add_child(.x = dat, .value = "oml:default_target_attribute", default_target)
  }
  if (!is.null(row_identifier)) {
    xml2::xml_add_child(.x = dat, .value = "oml:row_id_attribute", row_identifier)
  }
  if (!is.null(ignore)) {
    for (ia in ignore) {
      xml2::xml_add_child(.x = dat, .value = "oml:ignore_attribute", ia)
    }
  }
  if (!is.null(citation)) {
    xml2::xml_add_child(.x = dat, .value = "oml:citation", citation)
  }
  if (!is.null(original_data_url)) {
    xml2::xml_add_child(.x = dat, .value = "oml:original_data_url", original_data_url)
  }
  if (!is.null(paper_url)) {
    xml2::xml_add_child(.x = dat, .value = "oml:paper_url", paper_url)
  }

  withr::defer(unlink(desc_path))
  desc_path = tempfile(fileext = ".xml")
  xml2::write_xml(x = doc, file = desc_path)

  withr::defer(unlink(data_path))
  data_path = tempfile(fileext = ".arff")
  foreign::write.arff(x, data_path)

  response = httr::POST(
    url = sprintf("%s/data", mlr3oml:::get_server()),
    body = list(
      description = httr::upload_file(desc_path),
      dataset = httr::upload_file(data_path)
    ),
    query = list(api_key = api_key)
  )
  response_list = xml2::as_list(httr::content(response))

  if (httr::http_error(response)) {
    warningf(
      paste(response_list$error$message, response_list$error$additional_information, collapse = "\n")
    )
    return(response_list)
  } else {
    id = as.integer(response_list$upload_data_set$id[[1L]] )
    return(id)
  }
}
