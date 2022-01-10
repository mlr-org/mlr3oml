truncate_name = function(name, width = 30L) {
  ifelse(nchar(name) < width, name, paste0(strtrim(name, width), "..."))
}

truncate_vector = function(name, width = 30L) {
  if (length(name) == 1) {
    return(ifelse(nchar(name) < width, name, paste0(strtrim(name, width), "...")))
  }
  name = name[[1]]
  ifelse(nchar(name) < width, name, paste0(strtrim(name, width), ", ..."))
}
