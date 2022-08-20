download_task_splits = function(task_id, server, desc = download_desc_task(task_id, server)) {
  splits = get_arff(desc$input$estimation_procedure$data_splits_url)
  splits$rowid = as.integer(splits$rowid)
  colnames(splits)[colnames(splits) == "repeat."] = "rep"
  splits
}
