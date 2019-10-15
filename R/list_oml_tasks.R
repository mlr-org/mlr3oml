list_oml_tasks = function(...) {
  dots = qassertr(list(...), "A1")

  query = paste("https://www.openml.org/api/v1/json/task/list",
    paste(names(dots), unlist(dots), sep = "/", collapse = "/"), sep = "/")
  j = jsonlite::fromJSON(query)
  setDT(j$tasks$task)[]
}
