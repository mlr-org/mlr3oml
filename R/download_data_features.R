download_data_features = function(id) {
  # convert_type(description$features, type_map_data_features)
  features = get_json("https://www.openml.org/api/v1/json/data/features/%i", id)[[1L]][[1L]]
  features$index = as.integer(features$index)
  features$data_type = factor(features$data_type, levels = c("nominal", "numeric"))
  features$is_target = as.logical(features$is_target)
  features$is_ignore = as.logical(features$is_ignore)
  features$is_row_identifier = as.logical(features$is_row_identifier)
  features$number_of_missing_values = as.integer(features$number_of_missing_values)
  features$name = make.names(features$name)

  setDT(features, key = "index")[]
}
