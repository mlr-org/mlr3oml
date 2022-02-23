#' TODO: This should actually be a R6 Metaclass that generates classes for each id with the
#' respective parameters
LearnerRegrOML = R6Class("LearnerRegrOML",
  inherit = LearnerRegr,
  public = list(
    initialize = function() {
      param_set = paradox::ps(
        name = paradox::p_uty(),
        id = paradox::p_int()
      )
      super$initialize(id = "regr.oml", param_set = param_set)
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
)

LearnerClassifOML = R6Class("LearnerClassifOML",
  inherit = LearnerClassif,
  public = list(
    initialize = function() {
      param_set = paradox::ps(
        name = paradox::p_uty(),
        id = paradox::p_int()
      )
      super$initialize(id = "classif.oml", param_set = param_set)
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
)

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

construct_paramset = function(parameter) {
  names = make.names(parameter[["name"]])
  if (!all(names == parameter[["name"]])) {
    message("Parameter names of flow were converted to comply with R's variable naming conventions.")
  }
  params = map(seq_len(nrow(parameter)), function(x) paradox::p_uty())
  params = set_names(params, names)
  invoke(paradox::ps, .args = params)
}
