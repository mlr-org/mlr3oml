#' @title Publish on OpenML
#'
#' @description
#'   Publishes a  [mlr3::Learner] (as a Flow) or [mlr3::ResampleResult] (as a Run) to OpenML.
#'   Currently only resample results with atomic parameters can be uploaded, as it has to be
#'   possible to represent the parameter values in json format.
#' @param x (`any`) Object to publish to OpenML.
#' @param confirm (`logical(1)`) Whether to confirm the upload.
#' @param ... Additional arguments.
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

  empty_learner = learner$clone(TRUE)
  empty_learner$reset()
  empty_learner$param_set$values = list()
  # ideally this resets the param values to the defaults but this is currently not possible

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
  is.null(get_private(learner)$oml$info)
}

#' @export
publish.Task = function(x, ...) { # nolint
  stop("Not implemented yet!")
}

#' @export
publish.Resampling = function(x, ...) { # nolint
  stop("Not possible with current OpenML API.")
}

#' @export
publish.ResampleResult = function(x, upload_model = FALSE, ...) { # nolint
  learner = x$learner
  if (!all(map_lgl(learner$param_set$values, is.atomic))) {
    stopf("Can currently only publish flows with atomic parameter values.")
  }
  task = x$task
  resampling = x$resampling
  # First we check whether the task and the resampling are from OpenML and that they have not
  # been modified since the download
  resampling_id = get_id(resampling)
  task_id = get_id(task)
  if (is.null(resampling_id) || is.null(task_id)) {
    stopf(
      paste0(
        "Can only publish ResampleResults using a (Task, Resampling) combination obtained from\n",
        "OpenML, as currently no custom data-splits can be uploaded to OpenML."
      )
    )
  }

  flow_id = publish(learner, confirm = FALSE)

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

  run_id = upload(
    url = url,
    body = list(
      description = httr::upload_file(desc_path),
      predictions = httr::upload_file(pred_path)
    )
  )
  return(list(run_id = run_id, flow_id = flow_id, task_id = task_id))
}
