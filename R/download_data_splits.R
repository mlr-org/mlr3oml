download_data_splits = function(id, description = download_task_description(id)) {
  ep = description$input$estimation_procedure
  path = file.path(tempdir(), sprintf("oml_data_splits_%i.arff", id))
  on.exit(file_rm(path))
  download.file(ep$data_splits_url, path, quiet = !getOption("mlr3oml.verbose", TRUE))

  read_arff(path)
}
