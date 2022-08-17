#' @export
publish.Learner = function(x, api_key = get_api_key(), ...) { # nolint
  if (inherits(x, "LearnerSurv")) {
    stopf("mlr3proba currently not supported")
  }

  empty_learner = learner$clone(TRUE)
  empty_learner$reset()
  empty_learner$param_set$values = list()
  # FIXME: ideally this resets the param values to the defaults but this is currently not possible

  description = describe_learner(x)
  desc_file = tempfile(fileext = ".xml")
  withr::defer(unlink(desc_file))
  xml2::write_xml(x = description, file = desc_file)


  url = paste0(get_server(), "/flow")
  response = httr::POST(
    url = sprintf("%s/flow", get_server()),
    body = list(
      description = httr::upload_file(desc_file)
    ),
    query = list(api_key = api_key)
  )
  response_list = xml2::as_list(httr::content(response))

  if (isTRUE(response_list$error$code[[1L]] == "171")) {
    info = response_list$error$additional_information[[1L]]
    id = as.integer(substr(info, 19, nchar(info)))
    messagef("Flow already exists with id %s.", id)
    return(id)
  } else if (httr::http_error(response)) {
    warningf(
      paste(response_list$error$message, response_list$error$additional_information, collapse = "\n")
    )
    return(response)
  } else {
    return(as.integer(response_list$upload_flow$id[[1L]]))
  }
}


describe_learner = function(x, ...) { # nolint
  R_version = paste0("R_", paste0(R.Version()[c("major", "minor")], collapse = "."))
  dependencies = paste(R_version, get_dependencies(x$packages), sep = ", ")
  external_version = calculate_hash(list(class(x), dependencies))
  name = sprintf("mlr3.%s", x$id)

  description = sprintf(
    "Learner %s from package(s) %s.", x$id,
    paste(x$packages, collapse = ", ")
  )
  ps = as.data.table(x$param_set)

  doc = xml2::xml_new_document()
  flow = xml2::xml_add_child(doc, "oml:flow", "xmlns:oml" = "http://openml.org/openml")
  xml2::xml_add_child(.x = flow, .value = "oml:name", name)
  xml2::xml_add_child(.x = flow, .value = "oml:external_version", external_version)
  xml2::xml_add_child(.x = flow, .value = "oml:description", description)
  xml2::xml_add_child(.x = flow, .value = "oml:dependencies", dependencies)
  for (i in seq_len(nrow(ps))) {
    par = xml2::xml_add_child(flow, .value = "oml:parameter")
    xml2::xml_add_child(.x = par, .value = "oml:name", ps[[i, "id"]])
    xml2::xml_add_child(.x = par, .value = "oml:data_type", ps[[i, "class"]])
    repr = if (inherits(ps[[i, "default"]], "NoDefault")) {
      jsonlite::toJSON("<NoDefault>")
    } else {
      jsonlite::toJSON(ps[[i, "default"]])
    }

    xml2::xml_add_child(.x = par, .value = "oml:default_value", repr)
  }
  return(doc)
}

# Gets the dependencies in the form "mlr3_x.x.x, rpart_x.x.x" from the packages.
get_dependencies = function(x) {
  versions = map(
    x,
    function(x) getNamespace(x)$.__NAMESPACE__.$spec["version"]
  )
  dependencies = stringi::stri_join(x, unlist(versions), sep = "_")
  dependencies = stringi::stri_join(dependencies, collapse = ", ")
  return(dependencies)
}
