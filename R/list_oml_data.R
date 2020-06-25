#' @export
list_oml_data = function(...) {
  dots = qassertr(list(...), "A1")

  query = paste("https://www.openml.org/api/v1/json/data/list",
    paste(names(dots), unlist(dots), sep = "/", collapse = "/"), sep = "/")
  j = get_json(query)
  tab = setDT(j$data$dataset)

  qualities = transpose_name_value(tab$quality)
  rcbind(remove_named(tab, "quality"), qualities)
}
