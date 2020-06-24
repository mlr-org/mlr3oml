download_data_description = function(id) {
  desc = jsonlite::fromJSON(sprintf("https://www.openml.org/api/v1/json/data/%i", id))[[1L]]
  desc$default_target_attribute = make.names(desc$default_target_attribute)
  desc$description = NULL

  desc
}
