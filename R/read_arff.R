#' @import stringi
read_arff = function(path) {
  #' @useDynLib mlr3oml c_parse_arff_levels
  parse_arff_levels = function(str) {
    .Call(c_parse_arff_levels, str)
  }

  #' @useDynLib mlr3oml c_remove_comment
  remove_comment = function(lines) {
    .Call(c_remove_comment, lines)
  }

  unquote = function(x) {
    i = stri_startswith_fixed(x, "'") & stri_endswith_fixed(x, "'")
    x[i] = stri_sub(x[i], 2L, -2L)
    x
  }

  con = file(path, "r")
  on.exit(close(con))

  # parse the header, store declarations
  declarations = character()
  repeat {
    line = stri_trim_left(readLines(con, n = 1L, ok = TRUE, warn = FALSE))

    if (length(line) == 0L) {
      stop("No @data section found")
    }

    if (stri_startswith_fixed(line, "@data", case_insensitive = TRUE)) {
      break
    }

    if (stri_startswith_fixed(line, "@attribute", case_insensitive = TRUE)) {
      declarations = c(declarations, stri_trim_right(line))
    }
  }

  # extract @attribute declarations as nx3 matrix
  declarations = stri_split_regex(declarations, "[[:space:]]+", n = 3L, simplify = TRUE)

  # extract names, unquote + clean
  col_names = make.names(unquote(declarations[, 2L]))
  if (anyDuplicated(col_names)) {
    stop("Duplicated column names detected after conversion")
  }

  # extract and translate col classes
  col_classes = declarations[, 3L]
  is_factor = stri_startswith_fixed(col_classes, "{")
  lvls = set_names(lapply(col_classes[is_factor], parse_arff_levels), col_names[is_factor])
  col_classes = ifelse(is_factor, "character", tolower(col_classes))

  col_classes = map_values(col_classes,
    old = c("real",    "numeric", "string",    "date"),
    new = c("numeric", NA,        "character", "character")
  )

  # read data in chunks with workaround for missing comment char functionality
  # this should go as soon as data.table supports a comment char
  max_lines = 10000L
  counter = 1L
  data = list()
  repeat {
    lines = readLines(con, n = max_lines, warn = FALSE, ok = TRUE)
    if (length(lines) == 0L)
      break

    data[[counter]] = fread(text = remove_comment(lines), col.names = col_names,
      sep = ",", quote = "'", na.strings = "?", blank.lines.skip = TRUE,
      header = FALSE, colClasses = unname(col_classes)
    )

    counter = counter + 1L
  }

  rm(lines)
  data = rbindlist(data, use.names = TRUE, fill = TRUE)

  # fix factor levels
  for (j in names(lvls)) {
    set(data, j = j, value = factor(data[[j]], levels = lvls[[j]]))
  }

  data
}
