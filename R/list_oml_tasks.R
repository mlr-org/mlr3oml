#' @rdname list_oml
#' @param task_id (`integer()`)\cr
#'   Vector of task ids to restrict to.
#' @export
list_oml_tasks = function(task_id = NULL, data_id = NULL, number_instances = NULL, number_features = NULL,
  number_classes = NULL, number_missing_values = NULL, tag = NULL, limit = getOption("mlr3oml.limit", 5000L), ...) {
  tab = get_paginated_table("task",
    task_id = task_id,
    data_id = data_id,
    number_instances = number_instances,
    number_features = number_features,
    number_classes = number_classes,
    number_missing_values = number_missing_values,
    tag = tag,
    limit = limit,
    ...
  )

  if (nrow(tab)) {
    setnames(tab, "did", "data_id")
    qualities = transpose_name_value(tab$quality, as_integer = TRUE)
    rcbind(remove_named(tab, c("task_type_id", "task_type_id", "format", "input", "quality")), qualities)
  }

  return(tab)
}
