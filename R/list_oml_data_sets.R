#' @title List Data Sets from OpenML
#'
#' @description
#' This function allows to find data sets on \url{https://openml.org/d} using
#' some simple filter criteria.
#'
#' Note that only a subset of filters is exposed here.
#' For a more feature-complete package, see \CRANpkg{OpenML}.
#'
#' @details
#' Filter values can be provided as single atomic values (typically integer or character).
#' Provide a numeric vector of length 2 (`c(l, u)`) to find matches in the range \eqn{[l, u]}.
#'
#' @param data_id (`integer()`)\cr
#'   Vector of data ids to restrict to.
#' @param number_instances (`integer()`)\cr
#'   Filter for number of instances.
#' @param number_features (`integer()`)\cr
#'   Filter for number of features.
#' @param number_classes (`integer()`)\cr
#'   Filter for number of labels of the target (only classification tasks).
#' @param number_missing_values (`integer()`)\cr
#'   Filter for number of missing values.
#' @param tag (`character()`)\cr
#'   Filter for specific tag. You can provide multiple tags as character vector.
#' @param limit (`integer()`)\cr
#'   Limit the results to `limit` records.
#'   Default is 5000.
#' @param ... (any)\cr
#'   Additional filters as named arguments.
#'
#' @return (`data.table()`) of results, or a Null data.table if no data set matches the criteria.
#'
#' @references
#' `r format_bib("openml_r", "vanschoren2014")`
#'
#' @export
#' @examples
#' \donttest{
#' list_oml_data_sets(number_instances = 150, number_features = c(1, 10))
#' }
list_oml_data_sets = function(data_id = NULL, number_instances = NULL, number_features = NULL,
  number_classes = NULL, number_missing_values = NULL, tag = NULL, limit = 5000L, ...) {
  limit = assert_count(limit, positive = TRUE, coerce = TRUE)

  dots = list(
    data_id = data_id,
    number_instances = number_instances,
    number_features = number_features,
    number_classes = number_classes,
    number_missing_values = number_missing_values,
    tag = tag
  )
  dots = insert_named(discard(dots, is.null), list(...))
  tab = get_paginated_table(dots, "data", limit)

  if (nrow(tab)) {
    setnames(tab, "did", "data_id")
    qualities = transpose_name_value(tab$quality, as_integer = TRUE)
    rcbind(remove_named(tab, c("md5_checksum", "file_id", "format", "quality")), qualities)
  }

  return(tab)
}
