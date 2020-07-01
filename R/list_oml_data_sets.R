#' @title List Data Sets from OpenML
#'
#' @description
#' This function allows to find data sets on \url{https://openml.org/d} using
#' some simple filter criteria.
#'
#' Note that only a subset of filters are exposed here.
#'
#' @details
#' Filter values can be provided as single atomic values (typically integer or character).
#' Provide a numeric vector of length 2 (`c(l, u)`) to find matches in the range \eqn{[l, u]}.
#'
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
#' @param ... (any)\cr
#'   Additional filters as named arguments.
#'
#' @return (`data.table()`) of results.
#' @export
#' @examples
#' \dontrun{
#' list_oml_data_sets(number_instances = 150, number_features = c(1, 10))
#' }
list_oml_data_sets = function(number_instances = NULL, number_features = NULL, number_classes = NULL,
  number_missing_values = NULL, tag = NULL, ...) {

  dots = list(number_instances = number_instances, number_features = number_features,
    number_classes = number_classes, number_missing_values = number_missing_values, tag = tag)
  dots = insert_named(discard(dots, is.null), list(...))

  query = build_filter_query("data", dots)
  tab = setDT(get_json(query)$data$dataset)
  setnames(tab, "did", "data_id")

  qualities = transpose_name_value(tab$quality, as_integer = TRUE)
  rcbind(remove_named(tab, c("md5_checksum", "file_id", "format", "quality")), qualities)
}
