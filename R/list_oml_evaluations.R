#' @title List Evaluations from OpenML
#'
#' @description
#' This function allows to find tasks on \url{https://openml.org/e} using
#' some simple filter criteria.
#' @export
list_oml_evaluations = function(run_id = NULL, tag = NULL, limit = 5000L, ...) {
  limit = assert_count(limit, positive = TRUE, coerce = TRUE)

  dots = list(
    run = run_id,
    tag = tag
  )
  dots = insert_named(discard(dots, is.null), list(...))
  tab = get_paginated_table(dots, "evaluation", limit)

  if (nrow(tab)) {
    tab[, c("upload_time", "array_data") :=
      list(strptime(get("upload_time"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), NULL)]
    setnames(tab, "function", "measure")
    tab = dcast(tab, ... ~ measure, value.var = "value")
  }

  return(tab[])
}

