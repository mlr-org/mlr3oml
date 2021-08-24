#' @rdname list_oml
#' @export
list_oml_measures = function() {
  measures = get_json("https://www.openml.org/api/v1/json/evaluationmeasure/list")[[c(1L, 1L, 1L)]]
  data.table(measure = measures)
}
