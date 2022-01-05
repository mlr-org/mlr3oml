#' @export
make_description = function(x, ...) {
  # TODO: Internal methods should probably not use S3
  UseMethod("make_description")
}

#' @export
make_description.default = function(x, ...) { # nolint
  stopf("Cannot create description for object of class %s.", class(x)[[1L]])
}

#' @export
make_description.Learner = function(x, ...) { # nolint
  name = sprintf("mlr3.%s_test", x$id)
  external_version = paste0(x$hash, "_test") # FIXME: remove this when new version is released
  dependencies = get_dependencies(x$packages)
  # TODO: remove this
  description = sprintf(
    "[TEST]: Learner %s from package(s) %s.", x$id,
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
    xml2::xml_add_child(
      .x = par, .value = "oml:default_value",
      format_default(ps[[i, "default"]])
    )
  }
  return(doc)
}


make_description.ResampleResult = function(x, ...) { # nolint
  assert(hasArg("flow_id") && hasArg("task_id"))
  args = list(...)

  doc = xml2::xml_new_document()
  run = xml2::xml_add_child(doc, "oml:run", "xmlns:oml" = "http://openml.org/openml")
  xml2::xml_add_child(.x = run, .value = "oml:task_id", args[["task_id"]])
  xml2::xml_add_child(.x = run, .value = "oml:flow_id", args[["flow_id"]])

  if (length(par_list <- x$learner$param_set$values)) {
    # TODO: take care of funtions
    for (i in seq_along(par_list)) {
      par = xml2::xml_add_child(run, .value = "oml:parameter_setting")
      xml2::xml_add_child(.x = par, .value = "oml:name", names(par_list)[[i]])
      xml2::xml_add_child(.x = par, .value = "oml:value", par_list[[i]])
    }
  }
  return(doc)
}

#' @export
# make_description.GraphLearner = function(x) {
#  name = sprintf("mlr3pipeline.%s", x$id)
#  dependencies = get_dependencies(x$packages)
#  description = sprintf("GraphLearner from package mlr3pipelines", x$id,
#                        paste(x$packages, collapse = ", "))
#
#  full_description = jsonlite::toJSON(x$graph$edges)
#
#  ps = as.data.table(x$param_set)
#  doc = xml2::xml_new_document()
#  flow = xml2::xml_add_child(doc, "oml:flow", "xmlns:oml" = "http://openml.org/openml")
#  xml2::xml_add_child(.x = flow, .value = "oml:name", name)
#  xml2::xml_add_child(.x = flow, .value = "oml:description", description)
#  xml2::xml_add_child(.x = flow, .value = "oml:dependencies", dependencies)
#  for (i in seq_len(nrow(ps))) {
#    par = xml2::xml_add_child(flow, .value = "oml:parameter")
#    xml2::xml_add_child(.x = par, .value = "oml:name", ps[[i, "id"]])
#    xml2::xml_add_child(.x = par, .value = "oml:data_type", ps[[i, "class"]])
#    xml2::xml_add_child(.x = par, .value = "oml:default_value",
#                        format_default(ps[[i, "default"]]))
#  }
#  xml2::xml_add_child(.x = flow, .value = "oml:full_description", full_description)
#  return(doc)
# }

format_default = function(x) {
  if (is.character(x) || is.numeric(x) || is.logical(x) || is.factor(x)) {
    return(x)
  }
  if (inherits(x, "NoDefault")) {
    return("<NoDefault>")
  }
}

#' Gets the dependencies in the form "mlr3_x.x.x, rpart_x.x.x" from the packages.
get_dependencies = function(x) {
  versions = mlr3misc::map(
    x,
    function(x) getNamespace(x)$.__NAMESPACE__.$spec["version"]
  )
  dependencies = stringi::stri_join(x, unlist(versions), sep = "_")
  dependencies = stringi::stri_join(dependencies, collapse = ", ")
  return(dependencies)
}

make_description.BenchmarkResult = function(x, ...) { # nolint
  doc = xml2::xml_new_document()
  args = list(...)
  if (!hasArg("description")) {
    # TODO: better default description
    args$description = sprintf("Benchmark result")
  }
  if (!hasArg("name")) {
    # TODO: better default name
    args$name = "mlr3.BenchmarkResult"
  }

  study = xml2::xml_add_child(doc, "oml:study", "xmlns:oml" = "http://openml.org/openml")
  xml2::xml_add_child(.x = study, .value = "oml:main_entity_type", "run")
  xml2::xml_add_child(.x = study, .value = "oml:name", name)
  xml2::xml_add_child(.x = study, .value = "oml:description", description)

  add_ids(study, args$task_ids, "task")
  add_ids(study, args$flow_ids, "flow")
  add_ids(study, args$run_ids, "run")

  return(doc)
}

add_ids = function(study, ids, type) {
  if (length(task_ids)) {
    objects = xml2::xml_add_child(study, .value = sprintf("oml:%ss", type))
    for (id in seq_len(ids)) {
      xml2::xml_add_child(.x = objects, sprintf("oml:%s_id", type), id)
    }
  }
  return(NULL)
}
