download_task_desc = function(task_id) {
  desc = get_json("https://www.openml.org/api/v1/json/task/%i", task_id, simplifyDataFrame = FALSE)[[1L]]

  desc$task_id = as.integer(desc$task_id)
  desc$task_type_id = as.integer(desc$task_type_id)
  desc$input = set_names(map(desc$input, function(x) x[[2L]]), map_chr(desc$input, "name"))
  desc$input$source_data$data_set_id = as.integer(desc$input$source_data$data_set_id)
  desc$output = NULL

  desc
}
