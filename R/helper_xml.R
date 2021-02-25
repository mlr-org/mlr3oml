get_oml_code = function(str) {
  str = stri_extract_first_regex(str, "<oml:code>\\s*\\d+\\s*</oml:code>")
  if (is.na(str)) {
    return(NA_integer_)
  }
  as.integer(stri_extract_first_regex(str, "\\d+"))
}

get_oml_message = function(str) {
  stri_trim_both(stri_replace_first_regex(str, ".*<oml:message>(.*)</oml:message>.*", "$1"))
}
