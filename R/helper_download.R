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


download_error = function(response) {
  stopf("Error downloading '%s' (http code: %i, oml code: %i, message: '%s'",
    response$url, response$http_code, response$oml_code, response$message)
}


download_file = function(url, path, api_key = get_api_key()) {
  lg$debug("Downloading to local file system", url = url, path = path, authenticated = !is.na(api_key))
  response = curl::curl_fetch_disk(add_auth_string(url, api_key = api_key), path)
  http_code = response$status_code
  ok = http_code == 200L
  oml_code = NA_integer_
  message = NA_character_

  if (!ok) {
    lg$debug("Server response not ok", http_code = http_code)
    content = stri_trim_both(paste0(readLines(path, warn = FALSE), collapse = "\n"))
    if (stri_startswith_fixed(content, "{")) {
      lg$debug("JSON response received", content = content)
      json = jsonlite::fromJSON(content)
      oml_code = as.integer(json$error$code)
      message = json$error$message
    } else if (stri_startswith_fixed(content, "<oml:error")) {
      lg$debug("XML response received", content = content)
      oml_code = get_oml_code(content)
      message = get_oml_message(content)
    } else {
      lg$debug("Unknown response received", content = content)
      message = content
    }
  }

  return(set_class(list(
    url = url,
    ok = ok,
    http_code = http_code,
    oml_code = oml_code,
    message = message
  ), "server_response"))
}


get_json = function(url, ..., simplify_vector = TRUE, simplify_data_frame = TRUE, api_key = get_api_key(), retries = 3L, error_on_fail = TRUE) { # nolint
  path = tempfile(fileext = ".json")
  on.exit(file.remove(path[file.exists(path)]))
  url = sprintf(url, ...)

  lg$info("Retrieving JSON", url = url, authenticated = !is.na(api_key))

  for (retry in seq_len(retries)) {
    response = download_file(url, path, api_key = api_key)

    if (response$ok) {
      json = jsonlite::fromJSON(path, simplifyVector = simplify_vector, simplifyDataFrame = simplify_data_frame)
      return(json)
    } else if (retry < retries) {
      if (response$oml_code %in% c(107L)) {
        delay = max(rnorm(1L, mean = 10), 0)
        lg$debug("Server busy, retrying in %.2f seconds", delay, try = retry)
        Sys.sleep(delay)
      } else {
        break
      }
    }
  }

  if (error_on_fail) {
    download_error(response)
  } else {
    return(response)
  }
}



get_arff = function(url, ..., sparse = FALSE, api_key = get_api_key(), retries = 3L) {
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
    } else if (retry < retries && response$http_code >= 500L) {
      delay = max(rnorm(1L, mean = 10), 0)
      lg$debug("Server busy, retrying in %.2f seconds", delay, try = retry)
      Sys.sleep(delay)
    }
  }

  download_error(response)
}

get_paginated_table = function(type, ..., limit) {
  limit = assert_count(limit, positive = TRUE, coerce = TRUE)
  dots = discard(list(...), is.null)
  chunk_size = magic_numbers$chunk_size
  tab = data.table()

  while (nrow(tab) < limit) {
    dots$limit = min(limit - nrow(tab), chunk_size)
    query = build_filter_query(type, dots)

    response = get_json(query, error_on_fail = FALSE)
    if (inherits(response, "server_response")) {
      if (response$oml_code %in% magic_numbers$oml_no_more_results) {
        # no more results
        break
      } else {
        download_error(response)
      }
    }

    response = response[[c(1L, 1L)]]
    response = setDT(map_if(response, is.data.frame, list))
    tab = rbind(tab, response)
    if (nrow(response) < dots$limit) {
      # fetched all results
      break
    }

    dots$offset = dots$offset %??% 0L + chunk_size
  }

  return(tab)
}
