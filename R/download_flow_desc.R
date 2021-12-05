download_flow_desc = function(flow_id) {
  server = getOption("mlr3oml.server") %??% "https://www.openml.org/api/v1"
  desc = get_json(paste0(server, "/json/flow/%i"), flow_id)[[1L]]
  desc$id = as.integer(desc$id)
  desc$upload_date = as.POSIXct(desc$upload_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  desc$parameter = as.data.table(desc$parameter)
  desc$version = as.integer(desc$version)
  desc$uploader = as.integer(desc$uploader)
  desc$id = as.integer(desc$id)
  desc$dependencies = stringi::stri_split(desc$dependencies, fixed = ", ")[[1]]
  if (!is.null(desc$full_description)) { # stores the graph for GraphLearner
    desc$full_description = as.data.table(jsonlite::fromJSON(desc$full_description))
  }
  return(desc)
}


if (FALSE) {
  desc = download_flow_desc(14745)
  desc
  desc$id = as.integer(desc$id)
  desc$uploader = as.integer(desc$uploader)
  # desc$upload
}
