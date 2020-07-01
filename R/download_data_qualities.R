download_data_qualities = function(data_id) {
  qualities = get_json("https://www.openml.org/api/v1/json/data/qualities/%i", data_id)[[1L]][[1L]]

  qualities$value = as.numeric(qualities$value)
  setDT(qualities, key = "name")[]
}
