download_data = function(data_id, desc = download_data_desc(data_id)) {
  # download file first so we get a progress bar :/
  path = file.path(tempdir(), sprintf("oml_data_%i.arff", data_id))
  on.exit(file_rm(path))
  download.file(desc$url, path, quiet = !getOption("mlr3oml.verbose", TRUE))

  data = read_arff(path)
  remove_named(data, c(desc$row_id_attribute, desc$ignore_attribute))
}
