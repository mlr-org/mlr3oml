#' @rdname list_oml
#' @param run_id (`integer()`)\cr
#'   Vector of run ids to restrict to.
#' @export
list_oml_runs = function(run_id = NULL, task_id = NULL, tag = NULL, flow_id = NULL,
  limit = limit_default(), test_server = test_server_default(), ...) {
  tab = get_paginated_table("run",
    run = run_id,
    task = task_id,
    tag = tag,
    flow = flow_id,
    limit = limit,
    server = get_server(test_server),
    ...
  )

  return(tab)
}
