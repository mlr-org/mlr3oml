#' @title Publish on OpenML
#'
#' @description
#' Publishes a {Flow, Run, Dataset, Task} to OpenML.
#' @details
#' This is a generic function.
#' @export
publish = function(x, confirm = TRUE, ...) {
  # TODO: remove this when package is ready
  # if (get_server() != "https://test.openml.org/api/v1") {

  if (confirm) {
    ask_confirmation()
  }

  if (TRUE) {
    id = query_existance(x)
    if (!is.null(id)) { # nolint
      return(id)
    }
  }
  id = UseMethod("publish", x)
  get_private(x)$oml_id = id
  return(id)
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
  api_key = get_api_key()
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
  id = upload(
    url = url,
    body = list(
      description = httr::upload_file(desc_file),
      binary = httr::upload_file(model_path)
    )
  )

  mlr3misc::messagef("Your flow was successfully uploaded and assigned id: %i.", id)

  return(flow_id)
}

publish.Task = function(x, resampling, ...) { # nolint
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

  flow_id = get_oml_id(learner) %??% publish(learner, confirm = FALSE)
  task_id = get_oml_id(task) %??% publish(task, resampling = resampling, confirm = FALSE)

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

  states_path = tempfile(fileext = ".rds")
  withr::defer(unlink(states_path))
  states = map(x$learners, "state")

  # TODO: Store only learner states, after downloading they can be reassambled using the binary
  # of the flow
  saveRDS(states, states_path)

  id = upload(
    url = url,
    body = list(
      description = httr::upload_file(desc_path),
      predictions = httr::upload_file(pred_path),
      binary = httr::upload_file(states_path)
    )
  )
  messagef("Your run was successfully uploaded and assigned id: %i.", id)
  output = list(run_id = run_id, flow_id = flow_id, task_id = task_id)
  return(output)
}


#' @export
publish.BenchmarkResult = function(x, ...) { # nolint
  flow_ids = map_int(x$learners$learner, publish, confirm = FALSE)
  task_ids = map_int(x$tasks$task, publish, confirm = FALSE)
  run_ids = map_int(x$resample_results$resample_result, publish, confirm = FALSE)

  # Now we create the study
  desc = make_description(x, ..., flow_ids = flow_ids, task_ids = task_ids, run_ids = run_ids)
  url = paste0(get_server(), "/run")
  desc_path = tempfile(fileext = ".xml")
}

get_oml_id = function(x) {
  get_private(x)$oml_id
}

get_task_id = function(task) {
  split = strsplit(task$backend$hash, "_")[[1L]]
  assert(split[[1L]] == "mlr3oml::task" && split[[2L]])
  return(split[[2L]])
}
