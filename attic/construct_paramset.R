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
