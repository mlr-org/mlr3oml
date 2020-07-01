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
