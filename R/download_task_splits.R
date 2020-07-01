download_task_splits = function(task_id, desc = download_task_desc(task_id)) {
  ep = desc$input$estimation_procedure
  splits = get_arff(ep$data_splits_url)

  splits$rowid = as.integer(splits$rowid)
  splits
}
