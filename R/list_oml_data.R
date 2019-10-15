list_oml_data = function(...) {
  dots = qassertr(list(...), "A1")

  query = paste("https://www.openml.org/api/v1/json/data/list",
    paste(names(dots), unlist(dots), sep = "/", collapse = "/"), sep = "/")
  j = jsonlite::fromJSON(query)
  setDT(j$data$dataset)[]
}
