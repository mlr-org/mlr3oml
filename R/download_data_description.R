download_data_description = function(id) {
  jsonlite::fromJSON(sprintf("https://www.openml.org/api/v1/json/data/%i", id))[[1L]]
}
