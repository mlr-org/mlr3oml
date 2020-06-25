download_task_description = function(id) {
  description = get_json("https://www.openml.org/api/v1/json/task/%i", id, simplifyDataFrame = FALSE)[[1L]]
  description$task_id = as.integer(description$task_id)
  description$input = set_names(map(description$input, function(x) x[[2L]]), map_chr(description$input, "name"))
  description$output = NULL
  description
}
