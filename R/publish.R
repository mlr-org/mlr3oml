#' @title Publish on OpenML
#'
#' @description
#' Publishes a {Flow, Run, Dataset, Task} to OpenML.
#' @details
#' This is a generic function.
#' @export
publish = function(x, confirm = TRUE, ...) {
  if (confirm) {
    ask_confirmation()
  }
  id = UseMethod("publish", x)
  x$.__enclos_env__$private$oml$id = id
  return(id)
}

#' @export
publish.default = function(x, ...) { # nolint
  stopf("Objects of class %s cannot be published.", class(x)[[1]])
}

#' @export
publish.Learner = function(x, ...) { # nolint
  if (!learner_is_publishable(x)) {
    warningf(
      paste0(
        "Learner cannot be published as the installed package versions don't match the\n",
        "versions under which the flow was created."
      )
    )
  }
  api_key = get_api_key()
  if (is.na(api_key)) stop("API key required for upload.")
  assert_character(api_key, any.missing = FALSE, len = 1)

  # we get the hash from the class of the learner,
  if (inherits(x, "LearnerSurv")) {
    require_namespaces("mlr3proba")
  }

  empty_learner = nullify_learner(x)

  description = make_description(empty_learner)
  desc_file = tempfile(fileext = ".xml")
  withr::defer(unlink(desc_file))
  xml2::write_xml(x = description, file = desc_file)

  model_path = tempfile(fileext = ".rds")
  saveRDS(empty_learner, model_path)

  url = paste0(get_server(), "/flow")
  id = upload(
    url = url,
    body = list(
      description = httr::upload_file(desc_file),
      binary = httr::upload_file(model_path)
    )
  )
  return(id)
}

learner_is_publishable = function(learner) {
  oml_id = get_private(learner)$oml$id
  if (is.null(oml_id) || test_count(oml_id)) {
    return(TRUE)
  }
  return(FALSE)
}

#' @export
publish.Task = function(x, resampling, ...) { # nolint
  stop("Not implemented yet!")
}

#' @export
publish.Resampling = function(x, resampling, ...) { # nolint
  stop("Not possible with current OpenML API.")
}

#' @title Publishes a ResampleResult
#' @param upload_model (`logical(1)`) Whether to upload the model with the resample result.
#' @export
publish.ResampleResult = function(x, upload_model = FALSE, ...) { # nolint
  learner = x$learner
  task = x$task
  resampling = x$resampling
  # First we check whether the task and the resampling are from OpenML and that they have not
  # been modified since the download
  resampling_id = get_id(resampling)
  task_id = get_id(task)
  if (is.null(resampling_id) || is.null(task_id)) {
    stopf("Aborting...")
  }
  # if (!)
  # TODO: Finish this, abort if not both have a oml id and were not changed in a relevant way.

  flow_id = publish(learner, confirm = FALSE)

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
  oml_pred = make_oml_prediction(x)
  # Note that it has to be this ARFF writer because with e.g. foreign::write.arff the evaluation
  # engine does not work (arff is not fully specified)
  farff::writeARFF(oml_pred, pred_path)

  states_path = tempfile(fileext = ".rds")
  withr::defer(unlink(states_path))
  states = map(x$learners, "state")
  if (!upload_model) {
    states = map(states, function(state) {
      state$model = NULL
      return(state)
    })
  }

  saveRDS(states, states_path)

  run_id = upload(
    url = url,
    body = list(
      description = httr::upload_file(desc_path),
      predictions = httr::upload_file(pred_path),
      binary = httr::upload_file(states_path)
    )
  )
  return(list(run_id = run_id, flow_id = flow_id, task_id = task_id))
}

nullify_learner = function(learner) {
  learner = learner$clone(TRUE)
  learner$reset()
  learner$param_set = NULL
  return(learner)
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
