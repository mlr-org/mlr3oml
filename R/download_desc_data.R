download_desc_data = function(data_id, server) {
  desc = get_json(paste0(server, "/json/data/%i"), data_id, simplify_data_frame = FALSE,
    server = server)[[1L]]
  parse_desc_data(desc)
}

parse_desc_data = function(desc) {
  desc$format = tolower(desc$format)
  if (desc$format %nin% c("arff", "sparse_arff")) {
    stopf("Unsupported data format: %s", desc$format)
  }

  desc$id = as.integer(desc$id)
  desc$version = as.integer(desc$version)
  desc$default_target_attribute = make.names(desc$default_target_attribute)
  desc$upload_date = strptime(desc$upload_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  desc$processing_date = strptime(desc$processing_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  # OpenML (sometimes) uploaded the ignore_attributes comma-seperated
  ignore_attribute = map(desc$ignore_attribute, function(x) strsplit(x, ",")[[1L]])
  desc$ignore_attribute = make.unique(make.names(unlist(ignore_attribute)))
  if (anyDuplicated(desc$ignore_attribute)) {
    stopf("No unique names after conversion. This happened because ignore attribute names are not valid R names and had to be converted, which created duplicates.") # nolint
  }
  return(desc)
}
