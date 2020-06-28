download_data_splits = function(task_id, desc = download_task_desc(task_id)) {
  ep = desc$input$estimation_procedure

  # download file first so we get a progress bar :/
  path = file.path(tempdir(), sprintf("oml_data_splits_%i.arff", task_id))
  on.exit(file_rm(path))
  download.file(ep$data_splits_url, path, quiet = !getOption("mlr3oml.verbose", TRUE))

  splits = read_arff(path)
  splits$rowid = as.integer(splits$rowid)
  splits
}
