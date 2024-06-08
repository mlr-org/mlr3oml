download_data_qualities = function(data_id, server) {
  qualities = get_json(paste0(server, "/datasets/qualities/%i"), data_id, server = server)

  qualities$value = as.numeric(qualities$value)
  setDT(qualities, key = "name")[]
}
