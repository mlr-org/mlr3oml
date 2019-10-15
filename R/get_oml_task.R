#' @export
get_oml_task = function(id) {
  id = assert_int(id, coerce = TRUE)
  j = jsonlite::fromJSON(sprintf("https://www.openml.org/t/%i/json", id))
  data = get_oml_data(as.integer(j$source$data_id))
  task_id = j$source_data$name

  switch(j$tasktype$name,
    "Supervised Classification" = mlr3::TaskClassif$new(task_id, data, target = j$target_feature),
    "Supervised Regression" = mlr3::TaskRegr$new(task_id, data, target = j$target_feature)
  )
}
