#' @title Publish on OpenML
#'
#' @description
#' Publishes a {Flow, Run, Dataset, Task} to OpenML.
#' @details
#' This is a generic function.
#' @export
publish = function(x, ...) {
  id = query_existance(x)
  if (id) { # nolint
    return(id)
  }
  UseMethod("publish", x)
}

publish.default = function(x, ...) { # nolint
  stopf("Objects of class %s cannot be published.", class(x)[[1]])
}

publish.AutoTuner = function(x, ...) { # nolint
  stopf("Objects of class %s cannot be published.", class(x)[[1]])
}

publish.GraphLearner = function(x, ...) { # nolint
  stopf("Objects of class %s cannot be published.", class(x)[[1]])
}

#' @export
publish.Learner = function(x, ...) { # nolint
  if (is.na(api_key)) stop("API key required for upload.")
  assert_character(api_key, any.missing = FALSE, len = 1)

  # Publish a new learner
  x = get(class(x)[[1]])$new()

  description = make_description(x)
  desc_file = tempfile(fileext = ".xml")
  withr::defer(unlink(desc_file))
  xml2::write_xml(x = description, file = desc_file)

  model_path = tempfile(fileext = ".rds")
  saveRDS(x, model_path)

  url = paste0(get_server(), "/flow")
  response = upload(
    url = url,
    body = list(
      description = httr::upload_file(desc_file),
      binary = httr::upload_file(model_path)
    )
  )

  flow_id = as.integer(xml2::as_list(response)$upload_flow$id[[1]])
  mlr3misc::messagef("Your flow was successfully uploaded and assigned id: %i.", flow_id)

  return(flow_id)
}

publish.Task = function(x) { # nolint
  # The plan here is as follows:
  # Tasks that are downloaded from OpenML have the private attribute `oml_id`.
  # When we upload a run we check for that attribute and for the time throw an error otherwise
  stop("Not implemented yet!")
}

#' @export
publish.ResampleResult = function(x, ...) { # nolint
  learner = x$learner
  task = x$task
  resampling = x$resampling

  flow_id = get_oml_id(learner) %??% publish(get(class(learner)[[1]])$new()) # TODO: Initialize a fresh learner
  task_id = get_oml_id(task) %??% publish(task, resampling = resampling)
  # TODO: Take care that we first create all the descriptions and check that this is working
  # and then upload everything, otherwise we might upload only part of it which is
  # undesirable

  url = paste0(get_server(), "/run")
  description = make_description(x, flow_id = flow_id, task_id = task_id)

  desc_path = tempfile(fileext = ".xml")
  withr::defer(unlink(desc_path))
  xml2::write_xml(x = description, file = desc_path)

  pred_path = tempfile(fileext = ".arff")
  withr::defer(unlink(pred_path))
  tab = as.data.table(x$prediction())
  # TODO: We need to add an index for the resampling iteration (?)
  foreign::write.arff(tab, pred_path)

  model_path = tempfile(fileext = ".rds")
  withr::defer(unlink(model_path))
  saveRDS(x$learners, model_path)

  response = upload(
    url = url,
    body = list(
      description = httr::upload_file(desc_path),
      predictions = httr::upload_file(pred_path),
      model_serialized = httr::upload_file(model_path)
    )
  )
  run_id = as.integer(xml2::as_list(response)$upload_run$run_id[[1]])
  messagef("Your run was successfully uploaded and assigned id: %i.", run_id)
  return(run_id)
}

get_oml_id = function(x) {
  return(get_private(x)$oml_id)
}

get_task_id = function(task) {
  split = strsplit(task$backend$hash, "_")[[1L]]
  assert(split[[1L]] == "mlr3oml::task" && split[[2L]])
  return(split[[2L]])
}
