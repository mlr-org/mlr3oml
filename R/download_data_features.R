download_data_features = function(data_id, server, desc = download_desc_data(data_id, server)) {
  features = get_json(paste0(server, "/json/data/features/%i"), data_id, server = server)[[1L]][[1L]]

  features$index = as.integer(features$index)
  features$name = make.names(features$name)
  if (anyDuplicated(features$name)) {
    # otherwise the renaming depends on the order which is not necessarily preserved everywhere
    stopf("No unique names after conversion. This happened because some column names are not valid R names and had to be converted, which created duplicates.") # nolint
  }
  features$data_type = factor(features$data_type, levels = c("nominal", "numeric", "string"))
  features$is_target = as.logical(features$is_target)
  features$is_row_identifier = as.logical(features$is_row_identifier)
  features$number_of_missing_values = as.integer(features$number_of_missing_values)

  # Fix for https://github.com/openml/OpenML/issues/1046:
  # we just overwrite the ignore attribute here
  features$is_ignore = features$name %in% desc$ignore_attribute

  setDT(features, key = "index")[]
}
