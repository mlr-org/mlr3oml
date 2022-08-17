publish_task = function(x, data_id, type, estimation_procedure, target = NULL, api_key = get_api_key(), ...) {

  assert_choice(type, c("classif", "regr"))
  assert_character(target, len = 1L, null.ok = TRUE)
  estimation_procedure = assert_integerish(estimation_procedure, len = 1L)
  data_ie = assert_integerish(data_id, len = 1L)

  task_type_id = switch(type,
    classif = "1",
    regr = "2"
  )
  doc = xml2::xml_new_document()
  task = xml2::xml_add_child(doc, "oml:task_inputs", "xmlns:oml" = "http://openml.org/openml")
  xml2::xml_add_child(task, "oml:task_type_id", task_type_id)
  xml2::xml_add_child(task, "oml:input", name = "source_data", data_id)
  if (!is.null(target)) {
    xml2::xml_add_child(task, "oml:input", name = "target_feature", target)
  }
  xml2::xml_add_child(task, "oml:input", name = "estimation_procedure",
    as.character(estimation_procedure)
  )

  withr::defer(unlink(desc_path))
  desc_path = tempfile(fileext = ".xml")
  xml2::write_xml(x = doc, file = desc_path)

  response = httr::POST(
    url = sprintf("%s/task", get_server()),
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
  } else {
    id = as.integer(response_list$upload_task$id[[1L]])
    return(id)
  }
}

