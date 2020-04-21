#' @export
as_data_backend.OMLData = function(data, primary_key = NULL, ...) {
  as_data_backend(data$data)
}

#' @export
as_data_backend.OMLTask = function(data, primary_key = NULL, ...) {
  as_data_backend(data$data$data)
}
