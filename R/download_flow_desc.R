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
    desc$parameter = as.data.table(
      desc$parameter)[, c("name", "data_type", "default_value")]
    desc$parameter[["name"]] = make.names(desc$parameter[["name"]])
    if (!(length(desc$parameter$name) == length(unique(desc$parameter$name)))) {
      stopf("Non-unique parameter names after conversion via make.names().")
    }
    desc$parameter$default_value = lapply(desc$parameter$default_value, parse_json_safely)
  }
  if (!is.null(desc$dependencies) && startsWith(desc$name, "mlr")) {
    desc$dependencies = stringi::stri_split(desc$dependencies, fixed = ", ")[[1]]
  } else {
    desc$dependencies = gsub("\n", ", ", desc$dependencies)
  }
  return(desc)
}
