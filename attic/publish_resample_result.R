#' @export
publish.ResampleResult = function(x, api_key = get_api_key(), ...) { # nolint
  if (isTRUE(nrow(x$errors) > 0)) {
    stopf("Cannot publish resample result containing errors.")
  }

  learner = x$learner
  task = x$task
  resampling = x$resampling
  # First we check whether the task and the resampling are from OpenML and that they have not
  # been modified since the download
  if (!isTRUE(task$hash == get_private(task)$oml$hash)) {
    stopf("You can only upload benchmark results using (unmodified) OpenML tasks.")
  }
  if (!isTRUE(resampling$hash == get_private(resampling)$oml$hash)) {
    stopf("You can only upload benchmark results using (unmodified) OpenML data splits.")
  }

  task_id = get_private(task)$oml$id
  flow_id = publish(learner, confirm = FALSE)

  url = paste0(get_server(), "/run")
  desc = describe_resample_result(x, flow_id = flow_id, task_id = task_id)

  desc_path = tempfile(fileext = ".xml")
  withr::defer(unlink(desc_path))
  xml2::write_xml(x = desc, file = desc_path)

  pred_path = tempfile(fileext = ".arff")
  withr::defer(unlink(pred_path))
  oml_pred = make_oml_prediction(x)
  # Note that it has to be this ARFF writer because with e.g. foreign::write.arff the evaluation
  # engine does not work (arff is not fully specified)
  farff::writeARFF(oml_pred, pred_path)

  response = httr::POST(
    url = url,
    body = list(
      description = httr::upload_file(desc_path),
      predictions = httr::upload_file(pred_path)
    ),
    query = list(api_key = api_key)
  )

  response_list = xml2::as_list(httr::content(response))

  if (httr::http_error(response)) { # here there are never duplicates it seems
    warningf(
      paste(response_list$error$message, response_list$error$additional_information, collapse = "\n")
    )
    return(response_list)
  } else {
    id = as.integer(response_list$upload_run$run_id[[1L]])
    return(id)
  }
}

describe_resample_result = function(x, task_id, flow_id) { # nolint
  doc = xml2::xml_new_document()
  run = xml2::xml_add_child(doc, "oml:run", "xmlns:oml" = "http://openml.org/openml")
  xml2::xml_add_child(.x = run, .value = "oml:task_id", task_id)
  xml2::xml_add_child(.x = run, .value = "oml:flow_id", flow_id)

  pars = x$learner$param_set$get_values()
  for (i in seq_along(pars)) {
    par = xml2::xml_add_child(run, .value = "oml:parameter_setting")
    xml2::xml_add_child(.x = par, .value = "oml:name", names(pars)[[i]])
    value = jsonlite::toJSON(pars[[i]])
    xml2::xml_add_child(.x = par, .value = "oml:value", value)
  }

  return(doc)
}
