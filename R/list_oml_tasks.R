#' @title List Tasks from OpenML
#'
#' @description
#' This function allows to find tasks on \url{https://openml.org/t} using
#' some simple filter criteria.
#'
#' Note that only a subset of filters is exposed here.
#' For a more feature-complete package, see \CRANpkg{OpenML}.
#'
#' @details
#' Filter values can be provided as single atomic values (typically integer or character).
#' Provide a numeric vector of length 2 (`c(l, u)`) to find matches in the range \eqn{[l, u]}.
#'
#' @inheritParams list_oml_data_sets
#'
#' @return (`data.table()`) of results.
#'
#' @references
#' \cite{mlr3oml}{openml-r}
#'
#' \cite{mlr3oml}{vanschoren2014}
#'
#' @export
#' @examples
#' \donttest{
#' list_oml_tasks(number_instances = 150, number_features = c(1, 10))
#' }
list_oml_tasks = function(number_instances = NULL, number_features = NULL, number_classes = NULL,
  number_missing_values = NULL, tag = NULL, limit = 5000L, ...) {

  dots = list(
    number_instances = assert_integerish(number_instances, lower = 1L, any.missing = FALSE, min.len = 1L, max.len = 2L, null.ok = TRUE, coerce = TRUE),
    number_features = assert_integerish(number_features, lower = 1L, any.missing = FALSE, min.len = 1L, max.len = 2L, null.ok = TRUE, coerce = TRUE),
    number_classes = assert_integerish(number_classes, lower = 1L, any.missing = FALSE, min.len = 1L, max.len = 2L, null.ok = TRUE, coerce = TRUE),
    number_missing_values = assert_integerish(number_missing_values, lower = 1L, any.missing = FALSE, min.len = 1L, max.len = 2L, null.ok = TRUE, coerce = TRUE),
    tag = assert_character(tag, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
  )
  limit = assert_count(limit, positive = TRUE, coerce = TRUE)
  dots = insert_named(discard(dots, is.null), list(...))

  chunk_size = 1000L
  tab = data.table()

  while(nrow(tab) < limit) {
    dots$limit = min(limit - nrow(tab), chunk_size)
    query = build_filter_query("task", dots)

    result = get_json(query, status_ok = 412L)
    if (is.null(result))
      break

    tab = rbind(tab, setDT(result$tasks$task))
    dots$offset = dots$offset %??% 0L + chunk_size
  }

  setnames(tab, "did", "data_id")

  qualities = transpose_name_value(tab$quality, as_integer = TRUE)
  rcbind(remove_named(tab, c("task_type_id", "task_type_id", "format", "input", "quality")), qualities)
}
