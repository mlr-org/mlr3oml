#' @rdname list_oml
#' @param task_id (`integer()`)\cr
#'   Vector of task ids to restrict to.
#' @param type (`character(1)`)\cr
#'   The task type, supported values are: `"clasisf"`, `"regr"`, `"surv"` and `"clust"`.
#' @export
list_oml_tasks = function(task_id = NULL, data_id = NULL, number_instances = NULL, number_features = NULL,
  number_classes = NULL, number_missing_values = NULL, tag = NULL, limit = limit_default(),
  test_server = test_server_default(), type = NULL, ...) {
  if (!is.null(type)) {
    assert_choice(type, c("classif", "regr", "surv", "clust"))
    type = switch(type,
      classif = "1",
      regr = "2",
      surv = "7",
      clust = "5"
    )
  }
  tab = get_paginated_table("task",
    task_id = task_id,
    data_id = data_id,
    number_instances = number_instances,
    number_features = number_features,
    number_classes = number_classes,
    number_missing_values = number_missing_values,
    tag = tag,
    limit = limit,
    server = get_server(test_server),
    type = type,
    ...
  )

  if (nrow(tab)) {
    setnames(tab, "did", "data_id")
    qualities = transpose_name_value(tab$quality, as_integer = TRUE)
    rcbind(remove_named(tab, c("task_type_id", "task_type_id", "format", "input", "quality")), qualities)
  }

  return(tab)
}
