download_data_desc = function(data_id) {
  server = get_server()
  desc = get_json(paste0(server, "/json/data/%i"), data_id,
    simplify_data_frame = FALSE
  )[[1L]]
  desc = parse_data_desc(desc)
  return(desc)
}

parse_data_desc = function(desc) {
  desc$format = tolower(desc$format)
  if (desc$format %nin% c("arff", "sparse_arff")) {
    stopf("Unsupported data format: %s", desc$format)
  }

  desc$id = as.integer(desc$id)
  desc$version = as.integer(desc$version)
  desc$default_target_attribute = make.names(desc$default_target_attribute)
  desc$upload_date = strptime(desc$upload_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  desc$processing_date = strptime(desc$processing_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  # remove_named(desc, c("file_id", "description", "md5_checksum"))

  # OpenML uploaded the ignore_attributes comma-seperated
  contains_comma = grepl(",", desc$ignore_attribute, fixed = TRUE)
  ignore_no_comma = desc$ignore_attribute[!contains_comma]
  ignore_comma = desc$ignore_attribute[contains_comma]
  ignore_comma = map(ignore_comma, function(x) strsplit(x, ",")[[1L]])
  ignore_comma = unlist(ignore_comma)
  desc$ignore_attribute = c(ignore_no_comma, ignore_comma)
  return(desc)
}
