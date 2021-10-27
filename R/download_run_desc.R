download_run_desc = function(run_id) {
  desc = get_json("https://www.openml.org/api/v1/json/run/%i", run_id)[[1L]]

  desc$run_id = as.integer(desc$run_id)
  desc$uploader = as.integer(desc$uploader)
  desc$task_id = as.integer(desc$task_id)
  desc$flow_id = as.integer(desc$flow_id)
  # desc$task_type_id = as.integer(desc$task_type_id)
  # desc$input = set_names(map(desc$input, function(x) x[[2L]]), map_chr(desc$input, "name"))
  # desc$input$source_data$data_set_id = as.integer(desc$input$source_data$data_set_id)
  # desc$output = NULL

  desc
}

# options("mlr3oml.api_key" = "91141e224e7b45a0d268ac52ad9313b0")
# desc = download_run_desc(1)
