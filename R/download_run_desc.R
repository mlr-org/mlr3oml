download_run_desc = function(run_id) {
  server = getOption("mlr3oml.server") %??% "https://www.openml.org/api/v1"
  desc = get_json(paste0(server, "/json/run/%i"), run_id)[[1L]]

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
