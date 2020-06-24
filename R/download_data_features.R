download_data_features = function(id) {
  # convert_type(description$features, type_map_data_features)
  features = jsonlite::fromJSON(sprintf("https://www.openml.org/api/v1/json/data/features/%i", id))[[1L]][[1L]]
  features$is_target = as.logical(features$is_target)
  features$is_ignore = as.logical(features$is_ignore)
  features$is_row_identifier = as.logical(features$is_row_identifier)
  features$numer_of_missing_values = as.integer(features$number_of_missing_values)
  features$name = make.names(features$name)

  setDT(features, key = "name")[]
}
