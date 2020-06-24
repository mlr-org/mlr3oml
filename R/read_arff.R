#' @useDynLib mlr3oml c_parse_arff_levels
#' @import stringi
read_arff = function(path) {
  parse_arff_levels = function(str) {
    .Call(c_parse_arff_levels, str)
  }

  unquote = function(x) {
    i = stri_startswith_fixed(x, "'") & stri_endswith_fixed(x, "'")
    x[i] = stri_sub(x[i], 2L, -2L)
    x
  }

  assert_file_exists(path, access = "r")
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

  # extract @attribute declarations
  declarations = stri_split_regex(declarations, "[[:space:]]+", n = 3L)

  # extract names, unquote + clean
  col_names = make.names(unquote(mlr3misc::map_chr(declarations, 2L)))
  if (anyDuplicated(col_names)) {
    stop("Duplicated column names detected after conversion")
  }

  # extract col classes
  col_classes = map_chr(declarations, 3L)
  col_is_factor = stri_startswith_fixed(col_classes, "{")

  col_classes[!col_is_factor] = map_values(tolower(col_classes[!col_is_factor]),
    c("real", "string", "date"), c("numeric", "character", "character"))

  # read data with workaround for missing comment char functionality
  lines = readLines(con, n = -1L, warn = FALSE, ok = TRUE)
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
