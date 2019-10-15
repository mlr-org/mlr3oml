#' @useDynLib mlr3oml
#' @import stringi
read_arff = function(file) {
  unquote = function(x) {
    i = stri_startswith_fixed(x, "'") & stri_endswith_fixed(x, "'")
    x[i] = stri_sub(x[i], 2L, -2L)
    x
  }

  ### find @data line
  con = file(file, "r")
  dsep = 1L
  repeat {
    line = readLines(con, n = 1L, ok = TRUE, warn = FALSE)
    if (length(line) == 0L) {
      close(con)
      stop("No @data section found")
    }

    if (stri_startswith_fixed(stri_trim_left(line), "@data", case_insensitive = TRUE)) {
      break
    }

    dsep = dsep + 1L
  }

  ### read header, close file
  header = stri_trim_both(readLines(file, dsep - 1L))
  close(con)

  # extract @attribute declarations
  declarations = header[stri_startswith_fixed(header, "@attribute", case_insensitive = TRUE)]
  declarations = stri_split_regex(declarations, "[[:space:]]+", n = 3)

  # extract names, unquote + clean
  names = unquote(mlr3misc::map_chr(declarations, 2L))
  if (anyDuplicated(names)) {
    warning("Duplicated column names detected")
  }
  names = make.names(names, unique = TRUE)

  ### parse declarations
  translate_type = function(declaration) {
    type = declaration[3L]
    if (stri_startswith_fixed(type, "{")) {
      lvls = parse_arff_levels(type)
      vroom::col_factor(levels = lvls)
    } else {
      switch(type,
        "real" = vroom::col_double(),
        "string" = vroom::col_character(),
        "date" = vroom::col_datetime(),
        vroom::col_guess()
      )
    }
  }
  types = lapply(declarations, translate_type)

  ### read data
  vroom::vroom(
    file,
    delim = ",",
    comment = "%",
    quote = "'",
    escape_backslash = FALSE,
    na = "?",
    trim_ws = TRUE,
    skip = dsep,
    col_types = types,
    col_names = names
  )
}
