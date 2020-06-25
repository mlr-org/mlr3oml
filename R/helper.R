transpose_name_value = function(li) {
  tab = rbindlist(map(li, function(x) {
    if (!is.null(x) && all(dim(x))) {
      set_names(as.list(as.numeric(x$value)), x$name)
    } else {
      data.table(..dummy = 1)
    }
  }), fill = TRUE)

  remove_named(tab, "..dummy")
}

file_rm = function(path) {
  file.remove(path[file.exists(path)])
}

get_json = function(url, ..., simplifyVector = TRUE, simplifyDataFrame = TRUE) {
  path = tempfile(pattern = "json", fileext = "json")
  on.exit(file_rm(path))
  download.file(sprintf(url, ...), path, quiet = TRUE)
  jsonlite::fromJSON(path, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame)
}
