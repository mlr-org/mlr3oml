download_arff = function(data_id, server, desc = download_desc_data(data_id, server)) {
  data = get_arff(desc$url, sparse = (desc$format == "sparse_arff"), server = server)
  return(data)
}

#' Download and parse arff file.
#'
#' Downloads and parses the arff file for the given url.
#' Multiple retries are attempted in case the server is busy.
#' The dataset is first downloaded to a temporary directory and then parsed.
#' When using the internal parser (default) the names are also changed to comply with R's
#' variable naming scheme.
#' @noRd
get_arff = function(url, ..., sparse = FALSE, server, api_key = get_api_key(server), retries = 3L) {
  path = tempfile(fileext = ".arff")
  on.exit(file.remove(path[file.exists(path)]))
  url = sprintf(url, ...)

  lg$info("Retrieving ARFF", url = url, authenticated = !is.na(api_key))

  for (retry in seq_len(retries)) {
    response = download_file(url, path, api_key = api_key)

    if (response$ok) {
      lg$debug("Start processing ARFF file", path = path)
      parser = getOption("mlr3oml.arff_parser", "internal")

      if (sparse || parser == "RWeka") {
        if (!requireNamespace("RWeka", quietly = TRUE)) {
          stopf("Failed to parse arff file, install 'RWeka'")
        }
        tab = setDT(RWeka::read.arff(path))
      } else if (parser == "internal") {
        tab = read_arff(path)
      } else {
        stopf("Unknown parser '%s'", parser)
      }

      lg$debug("Finished processing ARFF file",
        nrow = nrow(tab), ncol = ncol(tab),
        colnames = names(tab)
      )

      return(tab)
    } else if (retry < retries && response$http_code >= 500L) {
      delay = max(rnorm(1L, mean = 10), 0)
      lg$debug("Server busy, retrying in %.2f seconds", delay, try = retry)
      Sys.sleep(delay)
    }
  }

  download_error(response)
}
