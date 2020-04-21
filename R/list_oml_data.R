list_oml_data = function(...) {
  dots = qassertr(list(...), "A1")

  query = paste("https://www.openml.org/api/v1/json/data/list",
    paste(names(dots), unlist(dots), sep = "/", collapse = "/"), sep = "/")
  j = jsonlite::fromJSON(query)
  setDT(j$data$dataset)[]
}


if (FALSE) {
  x = list_oml_data()
  unnest(x, "quality")

  xx = as.data.table(x$quality[[1]])
  xx$value = as.numeric(xx$value)
  dcast(xx, . ~ name, value.var = "value")


  rbindlist(map(x$quality, function(x) {
    if (all(dim(x))) {
      setNames(as.list(x$value), x$name)
    } else {
      NULL
    }
  }), fill = TRUE)
}
