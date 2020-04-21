download_data_qualities = function(id) {
  qualities = jsonlite::fromJSON(sprintf("https://www.openml.org/api/v1/json/data/qualities/%i", id))[[1L]][[1L]]
  qualities$value = as.numeric(qualities$value)
  setDT(qualities, key = "name")[]
}
