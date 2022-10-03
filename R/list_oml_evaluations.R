#' @rdname list_oml
#' @param measures (`character()`)\cr
#'   Vector of evaluation measures to restrict to.
#' @export
list_oml_evaluations = function(run_id = NULL, task_id = NULL, measures = NULL, tag = NULL,
  limit = limit_default(), test_server = test_server_default(), ...) {
  tab = get_paginated_table("evaluation",
    run = run_id,
    task = task_id,
    "function" = measures,
    tag = tag,
    limit = limit,
    server = get_server(test_server),
    ...
  )

  if (nrow(tab)) {
    tab[, c("upload_time", "array_data") :=
      list(as.POSIXct(get("upload_time"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), NULL)]
    setnames(tab, "function", "measure")
    tab = dcast(tab, ... ~ measure, value.var = "value")
  }

  return(tab[])
}
