download_data_description = function(id) {
  desc = get_json("https://www.openml.org/api/v1/json/data/%i", id)[[1L]]
  desc$id = as.integer(desc$id)
  desc$version = as.integer(desc$version)
  desc$default_target_attribute = make.names(desc$default_target_attribute)
  desc$description = NULL

  desc
}
