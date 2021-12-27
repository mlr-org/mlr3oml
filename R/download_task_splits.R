download_task_splits = function(ep) {
  splits = get_arff(ep$data_splits_url)

  splits$rowid = as.integer(splits$rowid)
  splits
}
