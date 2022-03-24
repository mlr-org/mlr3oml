download_run_binary = function(run_id, desc = download_run_desc(run_id)) {
  binary_url = desc$output_data$file$url[desc$output_data$file$name == "binary"]
  if (!is.null(binary_url)) {
    get_rds(desc$output_data$file$url[desc$output_data$file$name == "binary"])
  } else {
    return(NULL)
  }
}
