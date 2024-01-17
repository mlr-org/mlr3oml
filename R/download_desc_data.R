download_desc_data = function(data_id, server) {
  desc = get_json(paste0(server, "/datasets/%i"), data_id, simplify_data_frame = FALSE, server = server)
  parse_desc_data(desc)
}

parse_desc_data = function(desc) {
  desc$format = tolower(desc$format)
  if (desc$format %nin% c("arff", "sparse_arff")) {
    stopf("Unsupported data format: %s", desc$format)
  }

  # default_target_attribute, row_id_attribute and ignore_attribute have reutrn type arrau<string> in the json
  # schema. In the empty case, jsonlite returns these as a `list()` so we have to convert them to characters

  # When accessing a dataset where make.names() does not return unique names on the features, an error is thrown
  # at another part in the code, so we can change the name of the target column here without risk
  desc$default_target_attribute = make.names(desc$default_target_attribute)
  desc$row_id_attribute = as.character(desc$row_id_attribute)
  # OpenML (sometimes) uploaded the ignore_attributes comma-seperated
  ignore_attribute = map(desc$ignore_attribute, function(x) strsplit(x, ",")[[1L]])
  desc$ignore_attribute = make.names(as.character(ignore_attribute))

  if (anyDuplicated(desc$ignore_attribute)) {
    stopf("No unique names after conversion. This happened because ignore attribute names are not valid R names and had to be converted, which created duplicates.") # nolint
  }

  desc$upload_date = strptime(desc$upload_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  desc$processing_date = strptime(desc$processing_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

  desc$creator = as.character(desc$creator)
  desc$contributor = as.character(desc$contributor)
  desc$paper_url = as.character(desc$paper_url)
  desc$tag = as.character(desc$tag)

  return(desc)
}

