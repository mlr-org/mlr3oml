download_data_qualities = function(data_id, server) {
  qualities = get_json(paste0(server, "/json/data/qualities/%i"), data_id, server = server)[[1L]][[1L]] # nolint

  qualities$value = as.numeric(qualities$value)
  setDT(qualities, key = "name")[]
}
