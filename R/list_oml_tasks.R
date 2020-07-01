#' @title List Tasks from OpenML
#'
#' @description
#' This function allows to find tasks on \url{https://openml.org/t} using
#' some simple filter criteria.
#'
#' Note that only a subset of filters are exposed here.
#'
#' @details
#' Filter values can be provided as single atomic values (typically integer or character).
#' Provide a numeric vector of length 2 (`c(l, u)`) to find matches in the range \eqn{[l, u]}.
#'
#' @inheritParams list_oml_data_sets
#'
#' @return (`data.table()`) of results.
#' @export
list_oml_tasks = function(number_instances = NULL, number_features = NULL, number_classes = NULL,
  number_missing_values = NULL, tag = NULL, ...) {

  dots = list(number_instances = number_instances, number_features = number_features,
    number_classes = number_classes, number_missing_values = number_missing_values, tag = tag)
  dots = insert_named(discard(dots, is.null), list(...))

  query = build_filter_query("task", dots)
  tab = setDT(get_json(query)$tasks$task)
  setnames(tab, "did", "data_id")

  qualities = transpose_name_value(tab$quality, as_integer = TRUE)
  rcbind(remove_named(tab, c("task_type_id", "task_type_id", "format", "input", "quality")), qualities)
}
