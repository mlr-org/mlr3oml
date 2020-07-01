build_filter_query = function(type, filters) {
  assert_list(filters, names = "unique")

  filters = imap_chr(filters, function(x, name) {
    if (is.numeric(x)) {
      x = assert_integerish(x, min.len = 1L, max.len = 2L, any.missing = FALSE, .var.name = sprintf("filter value of '%s'", name), coerce = TRUE)
      paste0(name, "/", paste0(x, collapse = ".."))
    } else {
      assert_character(x, min.len = 1L, min.chars = 1L, any.missing = FALSE)
      paste0(name, "/", x, collapse = "/")
    }
  })

  paste0("https://www.openml.org/api/v1/json/", type, "/list/",
    paste0(filters, collapse = "/"))
}
