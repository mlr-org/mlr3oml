download_prediction = function(run_id, server, desc = download_desc_run(run_id, server)) {
  is_prediction = which(grepl("prediction", desc$output_data$file$name))
  if (length(is_prediction) > 1L) {
    stopf("More than one prediction file on OpenML.")
  }

  url = desc$output_data$file$url[is_prediction]
  prediction = get_arff(url, server = server)
  return(prediction)
}
