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
#' list_oml_data_sets(number_instances = 150, number_features = c(1, 10))
#' }
list_oml_data_sets = function(number_instances = NULL, number_features = NULL, number_classes = NULL,
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
    query = build_filter_query("data", dots)

    result = get_json(query, status_ok = 412L)
    if (is.null(result))
      break

    tab = rbind(tab, setDT(result$data$dataset))
    dots$offset = dots$offset %??% 0L + chunk_size
  }

  setnames(tab, "did", "data_id")

  qualities = transpose_name_value(tab$quality, as_integer = TRUE)
  rcbind(remove_named(tab, c("md5_checksum", "file_id", "format", "quality")), qualities)
}


if (FALSE) {
  query = build_filter_query("data", dots)
  tab = setDT(get_json(query, status_ok = 412L)$data$dataset)

  dots = list(limit = 10, offset = 10)
  dots = list(number_instances = c(150), limit = 10, offset = 51)

  dots = list(number_instances = c(150), limit = 20, offset = 0)
  dots = list(number_instances = c(150), limit = 20, offset = 20)
  dots = list(number_instances = c(150), limit = 20, offset = 40)
  dots = list(number_instances = c(150), limit = 20, offset = 51)

  url = "https://www.openml.org/api/v1/json/data/list/number_instances/150/limit/10/offset/51"
}
