#' @title Upload data to OpenML
#'
#' @description
#' Upload a dataset to OpenML.
#' This can also be achieved through the [website](https://openml.org).
#'
#' @param data ([`data.frame()`])\cr
#'   The data to upload.
#' @param name (`character(1)`)\cr
#'   The name of the dataset.
#' @param desc (`character(1)`)\cr
#'   The description of the dataset.
#' @param license (`character(1)`)\cr
#'   The license of the dataset
#' @param default_target (`character(1)`)\cr
#'   The default target variable.
#' @param citation (`character(1)`)\cr
#'   How to cite the dataset.
#' @param original_data_url (character(1))\cr
#'   The URL of the original data set.
#' @param paper_url (`character(1)`)\cr
#'   The URL of the paper describing the data set.
#' @param row_identifier (`character(1)`)\cr
#'   Whether any of the columns is a row identifier.
#' @param ignore_attribute (`character(1)`)\cr
#'   Which columns to ignore during modeling.
#' @template param_test_server
#' @template param_api_key
#'
#' @export
publish_data = function(data, name, desc, license = NULL, default_target = NULL, citation = NULL,
  row_identifier = NULL, ignore_attribute = NULL, original_data_url = NULL, paper_url = NULL,
  test_server = test_server_default(), api_key = NULL) {
  require_namespaces(c("xml2", "httr"))
  assert_flag(test_server)
  if (is.null(api_key)) {
    api_key = get_api_key(get_server(test_server))
  } else {
    assert_string(api_key)
  }
  assert_data_frame(data)
  assert_subset(unique(map_chr(data, function(x) class(x)[[1L]])), c("numeric", "integer", "factor", "character"))
  assert_string(name)
  assert_string(desc)
  assert_string(license, null.ok = TRUE)
  assert_string(default_target, null.ok = TRUE)
  assert_choice(default_target, colnames(data), null.ok = TRUE)
  assert_choice(row_identifier, colnames(data), null.ok = TRUE)
  assert_choice(ignore_attribute, colnames(data), null.ok = TRUE)
  assert_string(citation, null.ok = TRUE)
  assert_string(original_data_url, null.ok = TRUE)
  assert_string(paper_url, null.ok = TRUE)

  doc = xml2::xml_new_document()
  dat = xml2::xml_add_child(doc, "oml:data_set_description", "xmlns:oml" = "http://openml.org/openml")

  add = function(name, value) {
    if (!is.null(value)) {
      xml2::xml_add_child(.x = dat, .value = paste0("oml:", name), value)
    }
  }

  # Order matters!
  add("name", name)
  add("description", desc)
  add("format", "arff")
  add("licence", license)
  add("default_target_attribute", default_target)
  add("row_id_attribute", row_identifier)
  add("ignore_attribute", ignore_attribute)
  add("citation", citation)
  add("original_data_url", original_data_url)
  add("paper_url", paper_url)

  desc_path = tempfile(fileext = ".xml")
  withr::defer(unlink(desc_path))
  xml2::write_xml(x = doc, file = desc_path)

  data_path = tempfile("arff")
  withr::defer(unlink(data_path))
  write_arff(data, data_path)

  response = httr::POST(
    url = sprintf("%s/data", get_server(test_server)),
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
    return(response)
  }

  as.integer(response_list$upload_data_set$id[[1]])
}
