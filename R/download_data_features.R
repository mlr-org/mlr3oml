download_data_features = function(data_id, desc = download_task_desc(data_id)) {
  features = get_json("https://www.openml.org/api/v1/json/data/features/%i", data_id)[[1L]][[1L]]

  features$index = as.integer(features$index)
  features$name = make.names(features$name)
  features$data_type = factor(features$data_type, levels = c("nominal", "numeric"))
  features$is_target = as.logical(features$is_target)
  features$is_row_identifier = as.logical(features$is_row_identifier)
  features$number_of_missing_values = as.integer(features$number_of_missing_values)

  # Fix for https://github.com/openml/OpenML/issues/1046:
  # we just overwrite the ignore attribute here
  features$is_ignore = features$name %in% desc$ignore_attribute

  setDT(features, key = "index")[]
}
