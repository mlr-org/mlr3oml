#' Creates a pseudo OpenML Learner
#' This object is used to create A learner for a flow that is not from R, but the results are used
#' in a resample or benchmark result
make_oml_learner = function(flow, task_type) {
  if (task_type %nin% c("regr", "classif")) {
    warning("No learner (not even pseudo learner) could be constructed.")
    return(NULL)
  }
  learner = R6Class(sprintf("Learner%sOML%s", capitalize(task_type), flow$id),
    inherit = c(classif = LearnerClassif, regr = LearnerRegr)[[task_type]],
    public = list(
      initialize = function() {
        super$initialize(
          id = sprintf("%s.oml_%s", task_type, flow$id),
          param_set = construct_paramset(flow$parameter)
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

  messagef("Constructed non-executable pseudo learner %s.", learner$id)

  return(learner)
}

#' this constructs a paramset for the parameter field of a OMLFlow
construct_paramset = function(parameter) {
  names = make.names(parameter[["name"]])
  if (!all(names == parameter[["name"]])) {
    message("Parameter names of flow were converted to comply with R's variable naming conventions.")
  }
  params = map(seq_len(nrow(parameter)), function(x) paradox::p_uty())
  params = set_names(params, names)
  invoke(paradox::ps, .args = params)
}
