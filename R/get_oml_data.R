#' @export
get_oml_data = function(id) {
  id = assert_int(id, coerce = TRUE)
  j = jsonlite::fromJSON(sprintf("https://www.openml.org/d/%i/json", id))
  dirname = file.path(dirname(tempdir()), "oml_data")
  if (!dir.exists(dirname))
    dir.create(dirname)
  file = file.path(dirname, sprintf("%i.arff", id))
  if (!file.exists(file))
    download.file(j$url, file)
  setDT(read_arff(file))[]
}
