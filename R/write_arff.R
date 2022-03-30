#' @title Write ARFF files
#'
#' @description
#' Writes a `data.frame()` to an ARFF file.
#'
#' Limitations:
#' * Logicals are written as categorical features.
#' * [POSIXct] columns are converted to UTC.
#'
#' @param data (`data.frame()`)\cr
#'   Data to write.
#' @param path (`character(1)`)\cr
#'   Path or URI of the ARFF file, passed to [file()].
#' @param relation (`character(1)`)\cr
#'   Relation (name) of the data set.
#'
#' @return NULL
#' @export
write_arff = function(data, path, relation = deparse(substitute(data))) {
  assert_data_frame(data, min.rows = 1L, min.cols = 1L)
  assert_path_for_output(path, overwrite = TRUE)
  assert_string(relation)

  squote = function(s) {
    ifelse(is.na(s), s, sprintf("'%s'", gsub("(['\\])", "\\\\\\1", s)))
  }

  handle = file(path, "wt")
  on.exit(close(handle))

  line = sprintf("@relation '%s'", relation)
  writeLines(line, handle)

  for (cn in colnames(data)) {
    x = data[[cn]]
    cl = class(x)[1L]

    type = switch(cl,
      "logical" = {
        data[[cn]] = factor(x, levels = c("FALSE", "TRUE"))
        "{FALSE, TRUE}"
      },
      "integer" = "numeric",
      "numeric" = "numeric",
      "character" = "string",
      "factor" = {
        lvls = squote(levels(x))
        sprintf("{%s}", paste0(lvls, collapse = ","))
      },
      "Date" = {
        data[[cn]] = strftime(x, "%Y-%m-%d", tz = "UTC")
        "date \"yyyy-MM-dd\""
      },
      "POSIXct" = {
        data[[cn]] = strftime(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
        "date \"yyyy-MM-dd HH:mm:ss\""
      },
      default = stopf("Unsupported column format: '%s'", cl)
    )

    line = sprintf("@attribute %s %s", squote(cn), type)
    writeLines(line, handle)
  }

  writeLines("@data", handle)
  close(handle)
  on.exit()

  fwrite(data, file = path, na = "?", eol = "\n", append = TRUE,
    col.names = FALSE, row.names = FALSE)

  invisible(NULL)
}
