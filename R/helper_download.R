get_api_key = function() {
  key = getOption("mlr3oml.api_key") %??% Sys.getenv("OPENMLAPIKEY")
  if (nzchar(key))
    return(key)

  NA_character_
}

add_auth_string = function(url, api_key = get_api_key()) {
  if (is.na(api_key)) {
    return(url)
  }

  assert_string(api_key, min.chars = 32L)
  sprintf("%s?api_key=%s", url, api_key)
}


download_file = function(url, path, status_ok = integer(), api_key = get_api_key(), retries = 1L) {
  lg$debug("Downloading to local file system", url = url, path = path)

  for (retry in seq_len(retries)) {
    oml_code = NA_integer_
    res = curl::curl_fetch_disk(add_auth_string(url), path)
    status_code = res$status_code

    if (status_code %in% c(http_codes$ok, status_ok)) {
      return(res$status_code)
    }

    parsed = try(jsonlite::fromJSON(readLines(res$content, warn = FALSE)), silent = TRUE)
    if (inherits(parsed, "try-error")) {
      msg = substr(paste0(readLines(res$content, warn = FALSE), collapse = ""), 1L, 1000L)
      oml_code = get_oml_code(msg)
    } else {
      msg = parsed$error$message
    }

    if (retry < retries && oml_code %in% oml_codes$temp) {
      delay = abs(stats::rnorm(1L, mean = 5))
      lg$debug("Error downloading file, retrying in %.2f seconds", delay,
        url = url, status_code = status_code, msg = msg, oml_code = oml_code)
      Sys.sleep(delay)
    } else {
      stopf("Error downloading '%s' (status code: %i, message: '%s')", url, status_code, msg)
    }
  }
}


get_json = function(url, ..., simplify_vector = TRUE, simplify_data_frame = TRUE, status_ok = integer()) {
  path = tempfile(fileext = ".json")
  on.exit(file.remove(path[file.exists(path)]))
  url = sprintf(url, ...)

  api_key = get_api_key()
  lg$info("Retrieving JSON", url = url, authenticated = !is.na(api_key))

  status = download_file(url, path, status_ok = status_ok, api_key = api_key, retries = 3L)
  json = jsonlite::fromJSON(path, simplifyVector = simplify_vector, simplifyDataFrame = simplify_data_frame)
}


get_arff = function(url, sparse = FALSE, ...) {
  path = tempfile(fileext = ".arff")
  on.exit(file.remove(path[file.exists(path)]))
  url = sprintf(url, ...)

  download_file(url, path)

  lg$debug("Start processing ARFF file", path = path)
  parser = getOption("mlr3oml.arff_parser", "internal")

  if (sparse || parser == "RWeka") {
    if (!requireNamespace("RWeka", quietly = TRUE)) {
      stopf("Failed to parse arff file, install 'RWeka'")
    }
    tab = setDT(RWeka::read.arff(path))
  } else if (parser == "farff") {
    tab = setDT(utils::getFromNamespace("readARFF", ns = "farff")(path, show.info = FALSE))
  } else if (parser == "internal") {
    tab = read_arff(path)
  } else {
    stopf("Unknown parser '%s'", parser)
  }

  lg$debug("Finished processing ARFF file", nrow = nrow(tab), ncol = ncol(tab),
    colnames = names(tab))

  return(tab)
}

get_paginated_table = function(dots, type, limit = 5000L) {
  chunk_size = 1000L
  tab = data.table()

  while (nrow(tab) < limit) {
    dots$limit = min(limit - nrow(tab), chunk_size)
    query = build_filter_query(type, dots)

    result = get_json(query, status_ok = 412L)
    if (!is.null(result$error) && as.integer(result$error$code) %in% oml_codes$no_more_results) {
      # no more results
      break
    }

    new_rows = setDT(result[[1L]][[1L]])
    tab = rbind(tab, new_rows)
    if (nrow(new_rows) < limit) {
      # fetched all results
      break
    }

    dots$offset = dots$offset %??% 0L + chunk_size
  }

  return(tab)
}
