#' @rdname list_oml
#' @export
list_oml_measures = function(test_server = test_server_default()) {
  server = get_server(test_server)
  measures = get_json(sprintf("%s/json/evaluationmeasure/list", server),
    server = server)[[c(1L, 1L, 1L)]]
  data.table(measure = measures)
}
