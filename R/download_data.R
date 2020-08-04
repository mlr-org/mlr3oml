download_data = function(data_id, desc = download_data_desc(data_id)) {
  data = get_arff(desc$url, sparse = (desc$format == "sparse_arff"))

  remove_named(data, c(desc$row_id_attribute, desc$ignore_attribute))
}
