#' Creates a pseudo OpenML Learner
#' This object is used to create A learner for a flow that is not from R, but the results are used
#' in a resample or benchmark result
make_oml_learner = function(flow, task_type = NULL) {
  if (is.null(task_type) || task_type %nin% c("classif", "regr", "surv")) {
    warningf("Please provide a task tape ('regr', 'classif', 'surv').\nReturning NULL")
    return(NULL)
  }
  if (task_type == "surv") {
    require_namespaces("mlr3proba")
    super_class = mlr3proba::LearnerSurv
  } else {
    super_class = switch(task_type,
      regr = LearnerRegr,
      classif = LearnerClassif
    )
  }
  learner = R6Class(sprintf("Learner%sOML%i", capitalize(task_type), flow$id),
    inherit = super_class,
    public = list(
      initialize = function() {
        super$initialize(
          id = sprintf("%s.oml_%s", task_type, flow$id),
          param_set = construct_paramset(flow)
        )
      }
    ),
    private = list(
      .train = function(task) {
        stop("This is only a pseudo learner and cannot be trained.")
      },
      .predict = function(task) {
        stop("This is only a pseudo learner and cannot be used for prediction.")
      }
    )
  )$new()
  return(learner)
}

#' this constructs a paramset for the parameter field of a OMLFlow
#' the ids of the flows are appended to the names of the parameters to make their names unique
#' (A flow can contain various components which are themselves flows)
construct_paramset = function(flow) {
  # first we construct the parameters for the flow itself
  if (nrow(flow$parameters)) {
    names = paste(flow$parameters[["name"]], flow$id, sep = "_")
    params = map(seq_along(names), function(x) paradox::p_uty())
    params_flow = set_names(params, names)
  } else { # no params
    params_flow = list()
  }

  # early exit if there are no subcomponents
  if (is.null(flow$desc$component)) {
    if (!length(params_flow)) {
      return(paradox::ps())
    }
    # flow has params but no subcomponents
    param_set = invoke(paradox::ps, .args = params_flow)
    return(param_set)
  }

  # In this case (sklearn pipelines) the learner consists of other components, we append
  # these id to the parameter name, e.g. cp from component
  sub_ids = as.integer(flow$desc$component$flow$id)
  flows = map(seq_along(sub_ids),
    function(i) {
      parnames = OMLFlow$new(as.integer(flow$desc$component$flow$id[[i]]))
    }
  )
  # param_set_total = invoke(paradox::ps, .args = )


  names = map(seq_along(sub_ids),
    function(i) {
      parnames = OMLFlow$new(as.integer(flow$desc$component$flow$id[[i]]))$parameters[["name"]]
      paste0(parnames, "_", sub_ids[[i]])
    }
  )
  names = unlist(names)
  params = map(seq_along(names), function(x) paradox::p_uty())
  params_components = set_names(params, names)
  param_set_total = invoke(paradox::ps, .args = c(params_flow, params_components))
  return(param_set_total)
}
