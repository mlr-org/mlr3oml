download_desc_flow = function(flow_id, server) {
  desc = get_json(paste0(server, "/json/flow/%i"), flow_id, server = server)[[1L]]
  desc = parse_desc_flow(desc)
  return(desc)
}

parse_desc_flow = function(desc) {
  desc$id = as.integer(desc$id)
  desc$upload_date = as.POSIXct(desc$upload_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  desc$version = as.integer(desc$version)
  desc$uploader = as.integer(desc$uploader)
  desc$id = as.integer(desc$id)
  desc$parameter =

  if (is.null(desc$parameter)) {
    desc$parameter = data.table(
      name = character(0),
      data_type = character(0),
      default_value = list()
    )
  } else {
    desc$parameter = as.data.table(desc$parameter)[, c("name", "data_type", "default_value")]
  }
  return(desc)
}
