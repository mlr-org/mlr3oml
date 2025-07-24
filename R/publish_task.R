#' @title Publish a task on OpenML
#'
#' @description
#' Publish a task on OpenML.
#' This can also be achieved through the [website](https://www.openml.org).
#'
#' @param id (`integer(1)`)\cr
#'   The dataset id.
#' @param type (`character(1)` or `integer(1)`)\cr
#'   Can either be `"classif"` or `"regr"` or an integer indicating the task type.
#' @param estimation_procedure (`integer(1)`)\cr
#'   The id of the estimation procedure.
#' @param target (`character(1)`)\cr
#'   The target variable (if applicable).
#' @template param_api_key
#' @template param_test_server
#'
#' @export
publish_task = function(id, type, estimation_procedure, target, api_key = NULL,
  test_server = test_server_default()) {
  require_namespaces(c("xml2", "httr"))
  assert_flag(test_server)
  if (is.null(api_key)) {
    api_key = get_api_key(get_server(test_server))
  } else {
    assert_string(api_key)
  }
  assert_int(id, lower = 1L)
  if (test_character(type, len = 1L)) {
    type = switch(type,
      regr = 2,
      classif = 1,
      stopf("Invalid type '%s'.", type)
    )
  } else {
    assert_int(type, lower = 1L)
  }
  assert_character(target, len = 1L)
  estimation_procedure = assert_int(estimation_procedure)

  add = function(name, value) {
    if (!is.null(value)) {
      xml2::xml_add_child(.x = task, "oml:input", name = name, value)
    }
  }

  doc = xml2::xml_new_document()
  task = xml2::xml_add_child(doc, "oml:task_inputs", "xmlns:oml" = "http://openml.org/openml")
  xml2::xml_add_child(task, "oml:task_type_id", type)
  add("source_data", id)
  if (!is.null(target)) add("target_feature", target)
  add("estimation_procedure", estimation_procedure)

  withr::defer(unlink(desc_path))
  desc_path = tempfile(fileext = ".xml")
  xml2::write_xml(x = doc, file = desc_path)

  response = httr::POST(
    url = sprintf("%s/task", get_server(test_server)),
    body = list(
      description = httr::upload_file(desc_path)
    ),
    query = list(api_key = api_key)
  )

  response_list = xml2::as_list(httr::content(response))
  if (httr::http_error(response)) {
    if (isTRUE(response_list$error$code[[1L]] == "614")) { # Task already exists.
      info = response_list$error$additional_information[[1L]]
      id = as.integer(substr(info, 17L, nchar(info) - 1L))
      messagef("Task already exists with id %s.", id)
      return(id)
    } else {
      warningf(
        paste(response_list$error$message, response_list$error$additional_information, collapse = "\n")
      )
      return(response)
    }
  }

  as.integer(response_list$upload_task$id[[1L]])
}

