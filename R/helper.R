transpose_name_value = function(li) {
  tab = rbindlist(map(li, function(x) {
    if (!is.null(x) && all(dim(x))) {
      setNames(as.list(as.numeric(x$value)), x$name)
    } else {
      data.table(..dummy = 1)
    }
  }), fill = TRUE)

  remove_named(tab, "..dummy")
}
