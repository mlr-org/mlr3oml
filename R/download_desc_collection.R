download_desc_collection = function(collection_id, server) {
  desc = get_json(paste0(server, "/json/study/%i"), collection_id,
    simplify_data_frame = FALSE, server = server
  )[[1L]]

  parse_desc_collection(desc)
}

parse_desc_collection = function(desc) {
  desc$id = as.integer(desc$id)
  desc$creator = as.integer(desc$creator)
  desc$creation_date = as.POSIXct(desc$creation_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  if (!is.null(desc$flows)) desc$flows$flow_id = as.integer(desc$flows$flow_id)
  if (!is.null(desc$tasks)) desc$tasks$task_id = as.integer(desc$tasks$task_id)
  if (!is.null(desc$runs)) desc$runs$run_id = as.integer(desc$runs$run_id)
  if (!is.null(desc$data)) desc$data$data_id = as.integer(desc$data$data_id)
  return(desc)
}
