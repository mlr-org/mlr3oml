#' @noRd
local_test_server = function(env = parent.frame()) {
  op = options(mlr3oml.test_server = TRUE)
  withr::defer(options(op), env)
}

local_public_server = function(env = parent.frame()) {
  op = options(mlr3oml.test_server = FALSE)
  withr::defer(options(op), env)
}

local_log_info = function(env = parent.frame()) {
  prev_threshold = lg$threshold
  lg$set_threshold("info")
  withr::defer({lg$set_threshold(prev_threshold)}, env)
}

#' Returns the api key (if available) for the selected server.
#' @param server (`character(1)`)\cr
#'   The server to use.
#' @noRd
get_api_key = function(server) {
  test_server = server == get_server(TRUE)
  key = if (test_server) {
    getOption("mlr3oml.test_api_key", Sys.getenv("TESTOPENMLAPIKEY"))
  } else {
    getOption("mlr3oml.api_key", Sys.getenv("OPENMLAPIKEY"))
  }

  if (nzchar(key)) {
    return(key)
  }

  NA_character_
}

add_auth_string = function(url, api_key = NULL) {
  if (is.na(api_key)) {
    return(url)
  }

  assert_string(api_key, min.chars = 32L)
  sprintf("%s?api_key=%s", url, api_key)
}

download_error = function(response) {
  stopf(
    "Error downloading '%s' (http code: %i, oml code: %i, message: '%s'",
    response$url, response$http_code, response$oml_code, response$message
  )
}


download_file = function(url, path, api_key = NULL, server) {
  if (is.null(api_key)) {
    api_key = get_api_key(server)
  }
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


get_json = function(url, ..., simplify_vector = TRUE, simplify_data_frame = TRUE, server,
  api_key = get_api_key(server), retries = getOption("mlr3oml.retries", 3L), error_on_fail = TRUE) {
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


get_paginated_table = function(query_type, ..., limit, server) {
  limit = assert_count(limit, positive = TRUE, coerce = TRUE)
  dots = discard(list(...), is.null)
  chunk_size = magic_numbers$chunk_size
  tab = data.table()

  while (nrow(tab) < limit) {
    dots$limit = min(limit - nrow(tab), chunk_size)
    query = build_filter_query(query_type, dots, server)

    response = get_json(query, error_on_fail = FALSE, server = server)
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

with_cache = function(code, cache) {
  old_options = options(mlr3oml.cache = cache)
  on.exit({options(old_options)}, add = TRUE)
  force(code)
}
