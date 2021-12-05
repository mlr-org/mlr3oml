download_data_qualities = function(data_id) {
  server = getOption("mlr3oml.server") %??% "https://www.openml.org/api/v1"
  qualities = get_json(paste0(server, "/json/data/qualities/%i"), data_id)[[1L]][[1L]]

  qualities$value = as.numeric(qualities$value)
  setDT(qualities, key = "name")[]
}
