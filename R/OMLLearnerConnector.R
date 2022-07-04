OMLLearnerConnector = function(flow_id = NULL, task_type = NULL,
  cache = getOption("mlr3oml.cache", FALSE)) {
  as_learner(OMLFlow$new(flow_id, cache), task_type = task_type)
}
