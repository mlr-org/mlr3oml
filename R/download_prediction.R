download_prediction = function(run_id, desc = download_run_desc(run_id)) {
  is_prediction = which(grepl("prediction", desc$output_data$file$name))
  if (length(is_prediction) > 1L) {
    warningf("More than one prediction file, using the first.")
    is_prediction = is_prediction[[1L]]
  }

  url = desc$output_data$file$url[is_prediction]
  prediction = get_arff(url)
  colnames(prediction)[colnames(prediction) == "repeat."] = "rep"
  return(prediction)
}
