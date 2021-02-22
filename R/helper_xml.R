get_oml_code = function(str) {
  str = stri_extract_first_regex(str, "<oml:code>\\s*\\d+\\s*</oml:code>")
  if (is.na(str)) {
    return(NA_integer_)
  }
  as.integer(stri_extract_first_regex(str, "\\d+"))
}
