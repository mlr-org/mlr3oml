#' @title Read ARFF files
#'
#' @description
#' Parses a file located at `path` and returns a [data.table()].
#'
#' Limitations:
#'
#' * Only works for dense files, no support for sparse data.
#'   Use \CRANpkg{RWeka} instead.
#' * Dates (even if there is no time component) are read in as [POSIXct].
#' * The `date-format` from the ARFF specification is currently ignored.
#'   Instead, we rely on the auto-detection of \CRANpkg{data.table}'s [fread()]..
#'
#' @param path (`character(1)`)\cr
#'   Path or URI of the ARFF file, passed to [file()].
#'
#' @return ([data.table()]).
#' @import stringi
#' @export
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
    # this prevents double unquoting;
    # lines already unquoted w.r.t ' do not get unquote w.r.t. " again
    i = FALSE
    not_na = !is.na(x)

    for (quote in c("'", "\"")) {
      i = !i & not_na & stri_startswith_fixed(x, quote) & stri_endswith_fixed(x, quote)
      x[i] = stri_sub(x[i], 2L, -2L)
    }
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
  is_date = grepl("^date", col_classes, ignore.case = TRUE)
  lvls = set_names(lapply(col_classes[is_factor], parse_arff_levels), col_names[is_factor])

  ii = !is_factor & !is_date
  col_classes[ii] = tolower(col_classes[ii])
  col_classes[is_factor] = "character"
  col_classes[is_date] = "date"

  mapped_col_classes = map_values(col_classes,
    old = c("integer", "real", "numeric", "string", "date"),
    new = c(NA, NA, NA, "character", "POSIXct")
  )

  # read data in chunks with workaround for missing comment char functionality
  # this should go as soon as data.table supports a comment char
  data = vector("list", 100L) # over-allocating for 10M rows
  max_lines = 100000L
  quote_char = "\""
  lines = remove_comment(readLines(con, n = max_lines, warn = FALSE, ok = TRUE))
  counter = 1L

  while (length(lines) > 0L) {
    tmp = try(fread(
      text = lines, col.names = col_names,
      sep = ",", quote = quote_char, na.strings = "?", blank.lines.skip = TRUE,
      header = FALSE, colClasses = mapped_col_classes
    ), silent = TRUE)

    if (inherits(tmp, "try-error")) {
      if (quote_char != "'") {
        # try again with single quote
        quote_char = "'"
      } else {
        stop(tmp)
      }
    } else {
      data[[counter]] = tmp
      counter = counter + 1L
      lines = remove_comment(readLines(con, n = max_lines, warn = FALSE, ok = TRUE))
    }
  }

  data = if (counter == 2L) data[[1L]] else rbindlist(data, use.names = TRUE, fill = TRUE)

  for (j in which(col_classes == "integer")) {
    x = data[[j]]
    if (!is.integer(x) && test_integerish(x)) {
      set(data, j = j, value = as.integer(x))
    }
  }

  for (j in which(col_classes == "character")) {
    set(data, j = j, value = unquote(data[[j]]))
  }

  for (j in names(lvls)) {
    set(data, j = j, value = factor(data[[j]], levels = lvls[[j]]))
  }

  data
}
