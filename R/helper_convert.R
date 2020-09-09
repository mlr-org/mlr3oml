transpose_name_value = function(li, as_integer = FALSE) {
  converter = if (as_integer) as.integer else as.numeric
  tab = rbindlist(map(li, function(x) {
    if (!is.null(x) && all(dim(x))) {
      set_names(as.list(converter(x$value)), x$name)
    } else {
      data.table(..dummy = 1)
    }
  }), fill = TRUE)

  remove_named(tab, "..dummy")
}


new_task_classif = function(name, data, target) {
  y = data[[target]]
  if (!is.factor(y)) {
    data[[target]] = factor(y)
  }

  TaskClassif$new(name, data, target)
}


new_task_regr = function(name, data, target) {
  y = data[[target]]
  if (!is.numeric(y)) {
    data[[target]] = as.numeric(y)
  }

  TaskRegr$new(name, data, target)
}
