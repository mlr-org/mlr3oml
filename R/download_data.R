download_data = function(id, description = download_data_description(id)) {
  path = file.path(tempdir(), sprintf("oml_data_%i.arff", id))
  on.exit(file_rm(path))
  download.file(description$url, path, quiet = !getOption("mlr3oml.verbose", TRUE))

  data = read_arff(path)
  remove_named(data, c(description$row_id_attribute, description$ignore_attribute))
}
