#' @title List Runs from OpenML
#'
#' @description
#' This function allows to find tasks on \url{https://openml.org/r} using
#' some simple filter criteria.
#'
#' Note that only a subset of filters is exposed here.
#' For a more feature-complete package, see \CRANpkg{OpenML}.
#'
#' @param task_id (`integer()`)\cr
#'   Vector of task ids to restrict to.
#' @inheritParams list_oml_data_sets
#'
#' @return (`data.table()`) of results, or a Null data.table if no task matches the criteria.
#'
#' @references
#' `r format_bib("openml_r", "vanschoren2014")`
#'
#' @export
#' @examples
#' \donttest{
#' list_oml_tasks(number_instances = 150, number_features = c(1, 10))
#' list_oml_tasks(tag = "study_99")
#' }
list_oml_runs = function(run_id = NULL, task_id = NULL, tag = NULL, limit = 5000L, ...) {
  limit = assert_count(limit, positive = TRUE, coerce = TRUE)

  dots = list(
    run = run_id,
    task = task_id,
    tag = tag
  )
  dots = insert_named(discard(dots, is.null), list(...))
  tab = get_paginated_table(dots, "run", limit)

  return(tab)
}
