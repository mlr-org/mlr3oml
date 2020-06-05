find_data_marker = function(path) {
  con = file(path, "r")
  on.exit(close(con))

  dsep = 1L
  repeat {
    line = readLines(con, n = 1L, ok = TRUE, warn = FALSE)
    if (length(line) == 0L) {
      stop("No @data section found")
    }

    if (stri_startswith_fixed(stri_trim_left(line), "@data", case_insensitive = TRUE)) {
      return(dsep)
    }

    dsep = dsep + 1L
  }
}

#' @useDynLib mlr3oml c_parse_arff_levels
parse_arff_levels = function(str) {
  .Call(c_parse_arff_levels, str)
}

#' @import stringi
read_arff = function(path) {
  unquote = function(x) {
    i = stri_startswith_fixed(x, "'") & stri_endswith_fixed(x, "'")
    x[i] = stri_sub(x[i], 2L, -2L)
    x
  }

  dsep = find_data_marker(path)

  # read header, close file
  header = stri_trim_both(readLines(path, n = dsep - 1L))

  # extract @attribute declarations
  declarations = header[stri_startswith_fixed(header, "@attribute", case_insensitive = TRUE)]
  declarations = stri_split_regex(declarations, "[[:space:]]+", n = 3)

  # extract names, unquote + clean
  col_names = unquote(mlr3misc::map_chr(declarations, 2L))
  if (anyDuplicated(col_names)) {
    warning("Duplicated column names detected")
  }
  col_names = make.names(col_names, unique = TRUE)

  # extract col classes
  col_classes = map_chr(declarations, 3L)
  col_is_factor = stri_startswith_fixed(col_classes, "{")

  col_classes[!col_is_factor] = map_values(tolower(col_classes[!col_is_factor]),
    c("real", "string", "date"), c("numeric", "character", "character"))

  # read data with workaround for missing comment char functionality
  lines = tail(readLines(path), -dsep)
  lines = stri_trim_both(stri_split_fixed(lines, "%", 2L, simplify = TRUE)[, 1L])
  data = fread(text = lines, col.names = col_names,
    sep = ",", quote = "'", na.strings = "?", blank.lines.skip = TRUE,
    colClasses = ifelse(col_is_factor, "character", col_classes)
  )

  # fix factor levels
  for (j in which(col_is_factor)) {
    set(data, j = j, value = factor(data[[j]], levels = parse_arff_levels(col_classes[j])))
  }

  data
}
