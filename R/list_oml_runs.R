#' @rdname list_oml
#' @param run_id (`integer()`)\cr
#'   Vector of run ids to restrict to.
#' @export
list_oml_runs = function(run_id = NULL, task_id = NULL, tag = NULL, limit = getOption("mlr3oml.limit", 5000L), ...) {
  tab = get_paginated_table("run",
    run = run_id,
    task = task_id,
    tag = tag,
    limit = limit,
    ...
  )

  return(tab)
}
