#' @title List Taks from OpenML
#'
#' @description
#' This function is not yet finished.
#' You can pass filter criteria to construct an API request.
#'
#' @param ... (named `character()`)\cr
#'   Named values of filter criteria, e.g. `tag = "study_99"`.
#'
#' @return (`data.table()`) of results.
#' @export
list_oml_tasks = function(...) {
  dots = qassertr(list(...), "A1")

  query = paste("https://www.openml.org/api/v1/json/task/list",
    paste(names(dots), unlist(dots), sep = "/", collapse = "/"), sep = "/")
  j = get_json(query)
  tab = setDT(j$tasks$task)[]

  qualities = transpose_name_value(tab$quality)
  rcbind(remove_named(tab, c("input", "quality")), qualities)
}
