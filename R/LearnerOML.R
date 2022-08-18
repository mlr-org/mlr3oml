# Creates a pseudo OpenML Learner
# This object is used to create A learner for a flow that is not from R, but the results are used
# in a resample or benchmark result
make_oml_learner = function(flow, task_type = NULL) {
  if (is.null(task_type) || task_type %nin% c("regr", "classif", "surv")) {
    warningf("Please provide a task tape ('regr', 'classif', 'surv').\nReturning NULL")
    return(NULL)
  } else {
    if (task_type == "surv") {
      stopf("mlr3proba currently not supported.")
      # require_namespaces("mlr3proba")
      # super_class = mlr3proba::LearnerSurv
    } else {
      super_class = switch(task_type,
        regr = LearnerRegr,
        classif = LearnerClassif
      )
    }
    class_name = sprintf("Learner%sOML%i", capitalize(task_type), flow$id)
  }

  learner = R6Class(class_name,
    inherit = super_class,
    public = list(
      initialize = function() {
        super$initialize(
          id = sprintf("oml.%s", flow$id),
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
      },
      oml = NULL
    )
  )$new()

  learner$.__enclos_env__$private$oml = list(id = flow$id, info = "pseudo")

  return(learner)
}

# this constructs a paramset for the parameter field of a OMLFlow
# the ids of the flows are appended to the names of the parameters to make their names unique
# Note that this only works because in each flow, a flow can appear only once.
# Otherwise the mapping from component -> parameter (run$parameter_setting) would be
# ambiguous
construct_paramset = function(flow) {
  # first we construct the parameters for the flow itself
  if (nrow(flow$parameter)) {
    params = map(seq_len(nrow(flow$parameter)), function(x) paradox::p_uty())
    params_flow = set_names(params, paste0("f", flow$id, ".", flow$parameter$name))
    param_set = invoke(paradox::ps, .args = params_flow)
  } else {
    param_set = paradox::ps()
  }

  # exit condition
  if (is.null(flow$desc$component)) {
    return(param_set)
  }

  # In this case (sklearn pipelines) the learner consists of other components, we append
  # these id to the parameter name, e.g. cp from component
  component_ids = as.integer(flow$desc$component$flow$id)

  map(component_ids,
    function(id) {
      flow = OMLFlow$new(id)
      param_set$add(construct_paramset(flow))
    }
  )

  return(param_set)
}
