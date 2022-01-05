download_flow_desc = function(flow_id) {
  server = get_server()
  desc = get_json(paste0(server, "/json/flow/%i"), flow_id)[[1L]]
  desc = parse_flow_desc(desc)
  return(desc)
}

parse_flow_desc = function(desc) {
  desc$id = as.integer(desc$id)
  desc$upload_date = as.POSIXct(desc$upload_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  desc$version = as.integer(desc$version)
  desc$uploader = as.integer(desc$uploader)
  desc$id = as.integer(desc$id)


  if (is.null(desc$parameter)) {
    desc$parameter = data.table(
      name = character(0), data_type = character(0),
      default_value = list()
    )
  } else {
    desc$parameter = as.data.table(desc$parameter)[, list(name, data_type, default_value)]
  }
  if (!is.null(desc$dependencies)) {
    desc$dependencies = stringi::stri_split(desc$dependencies, fixed = ", ")[[1]]
  }
  if (!is.null(desc$full_description)) { # stores the graph for GraphLearner
    desc$full_description = as.data.table(jsonlite::fromJSON(desc$full_description))
  }
  return(desc)
}
