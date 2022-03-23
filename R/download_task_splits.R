download_task_splits = function(task_id, desc = download_task_desc(task_id)) {
  splits = get_arff(desc$input$estimation_procedure$data_splits_url)
  splits$rowid = as.integer(splits$rowid)
  colnames(splits)[colnames(splits) == "repeat."] = "rep"
  splits
}
