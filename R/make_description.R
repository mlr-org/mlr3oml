#' @export
make_description = function(x) {
  UseMethod("make_description")
}

#' @export
make_description.default = function(x) {
  stop("Not implemented")
}

#' @export
make_description.Learner = function(x) {
  # scheme:
  # - https://www.openml.org/api/v1/xsd/openml.implementation.upload
  # - https://www.openml.org/api/v1/xml_example/flow
  name = sprintf("mlr3.%s", x$id)
  dependencies = get_dependencies(x$packages)

  #if (flow_exists()) {
  #  print("flow exists")
  #}

  description = sprintf("Learner %s from package(s) %s.", x$id,
                        paste(x$packages, collapse = ", "))

  ps = as.data.table(x$param_set)

  doc = xml2::xml_new_document()
  flow = xml2::xml_add_child(doc, "oml:flow", "xmlns:oml" = "http://openml.org/openml")
  xml2::xml_add_child(.x = flow, .value = "oml:name", name)
  xml2::xml_add_child(.x = flow, .value = "oml:description", description)
  xml2::xml_add_child(.x = flow, .value = "oml:dependencies", dependencies)
  for (i in seq_len(nrow(ps))) {
    par = xml2::xml_add_child(flow, .value = "oml:parameter")
    xml2::xml_add_child(.x = par, .value = "oml:name", ps[[i, "id"]])
    xml2::xml_add_child(.x = par, .value = "oml:data_type", ps[[i, "class"]])
    xml2::xml_add_child(.x = par, .value = "oml:default_value",
                        format_default(ps[[i, "default"]]))
  }
  return(doc)
}

#' @export
make_description.GraphLearner = function(x) {
  name = sprintf("mlr3pipeline.%s", x$id)
  dependencies = get_dependencies(x$packages)

  #if (flow_exists()) {
  #  print("flow exists")
  #}
  description = sprintf("GraphLearner from package mlr3pipelines", x$id,
                        paste(x$packages, collapse = ", "))

  full_description = jsonlite::toJSON(x$graph$edges)

  ps = as.data.table(x$param_set)
  doc = xml2::xml_new_document()
  flow = xml2::xml_add_child(doc, "oml:flow", "xmlns:oml" = "http://openml.org/openml")
  xml2::xml_add_child(.x = flow, .value = "oml:name", name)
  xml2::xml_add_child(.x = flow, .value = "oml:description", description)
  xml2::xml_add_child(.x = flow, .value = "oml:dependencies", dependencies)
  for (i in seq_len(nrow(ps))) {
    par = xml2::xml_add_child(flow, .value = "oml:parameter")
    xml2::xml_add_child(.x = par, .value = "oml:name", ps[[i, "id"]])
    xml2::xml_add_child(.x = par, .value = "oml:data_type", ps[[i, "class"]])
    xml2::xml_add_child(.x = par, .value = "oml:default_value",
                        format_default(ps[[i, "default"]]))
  }
  xml2::xml_add_child(.x = flow, .value = "oml:full_description", full_description)
  return(doc)
}

format_default = function(x) {
  if (is.character(x) || is.numeric(x) || is.logical(x) || is.factor(x)) return(x)
  if (inherits(x, "NoDefault")) return("<NoDefault>")
}

#' Gets the dependencies in the form "mlr3_x.x.x, rpart_x.x.x" from the packages.
get_dependencies = function(x) {
  versions = mlr3misc::map(x,
                           function(x) getNamespace(x)$.__NAMESPACE__.$spec["version"])
  dependencies = stringi::stri_join(x, unlist(versions), sep = "_")
  dependencies = stringi::stri_join(dependencies, collapse = ", ")
  return(dependencies)
}
