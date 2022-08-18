with_test_server = function(env = parent.frame()) {
  op = options(
    mlr3oml.server = "https://test.openml.org/api/v1",
    mlr3oml.api_key = Sys.getenv("TESTOPENMLAPIKEY")
  )
  withr::defer(options(op), env)
}

with_public_server = function(env = parent.frame()) {
  op = options(
    mlr3oml.server = "https://openml.org/api/v1",
    mlr3oml.api_key = Sys.getenv("OPENMLAPIKEY")
  )
  withr::defer(options(op), env)
}

get_api_key = function() {
  key = getOption("mlr3oml.api_key", Sys.getenv("OPENMLAPIKEY"))
  if (nzchar(key)) { # neither option nor ENVIRONMENT variable is set
    return(key)
  }

  NA_character_
}

add_auth_string = function(url, api_key = NULL) {
  if (is.null(api_key)) {
    api_key = get_api_key()
  }
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


download_file = function(url, path, api_key = NULL) {
  if (is.null(api_key)) {
    api_key = get_api_key()
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


get_json = function(url, ..., simplify_vector = TRUE, simplify_data_frame = TRUE,
  api_key = get_api_key(), retries = 3L, error_on_fail = TRUE) {
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


get_rds = function(url, api_key = get_api_key(), retries = 3L) {
  path = tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  lg$info("Retrieving rds", url = url, authenticated = !is.na(api_key))
  for (retry in seq_len(retries)) {
    response = download_file(url, path, api_key = api_key)
    if (response$ok) {
      lg$debug("Start processing rds file", path = path)
      obj = tryCatch(readRDS(path),
        error = function(cond) {
          stopf("Could not read rds file.")
        }
      )
      return(obj)
    } else if (retry < retries && response$http_code >= 500L) {
      delay = max(rnorm(1L, mean = 10), 0)
      lg$debug("Server busy, retrying in %.2f seconds", delay, try = retry)
      Sys.sleep(delay)
    } else {
      stop("Error when downloading rds file.")
    }
  }
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

upload = function(url, body, query = list(api_key = get_api_key())) {
  response = httr::POST(url = url, query = query, body = body)
  content = httr::content(response)
  type = capitalize(tail(strsplit(url, "/")[[1]], n = 1))
  content_list = xml2::as_list(content)

  if (httr::http_error(response)) {
    error_message = content_list$error$message
    if (!is.null(error_message) && grepl("already exists", error_message)) {
      additional_info = content_list$error$additional_information
      idx = grep("implementation_id", additional_info)[[1]]
      id = as.integer(strsplit(additional_info[[idx]], split = ":")[[1]][2])
      messagef("%s already exists on OpenML with id %d.", type, id)
    } else {
      stop(xml2::as_list(httr::content(response))$error$message %??% "Unknown error")
    }
  } else {
    id = switch(tolower(type),
      flow = as.integer(content_list$upload_flow$id[[1]]),
      run = as.integer(content_list$upload_run$run_id[[1]])
    )
    messagef("Your %s was successfully uploaded and assigned id: %i.", type, id)
  }

  return(id)
}

#' @title Delete an object from OpenML
#' @description This function can be used to delete objects from the OpenML server.
#'
#' @param type (`character(1)`)
#'  What type of object should be deleted ("flow", "run", "task", "data", "study" / "collection")
#' @param id (`integer(1)`)
#'  The id of the object that will be deleted.
#' @param api_key (`character(1)`)
#'  The API key to perform the action, if left NULL it first tries getOption("mlr3oml.api_key") and
#'  then Sys.getenv("OMLAPIKEY").
#' @param server (`character(1)`)
#'  The server address, defaults to `get_server()`.
#' @param confirm (`logical(1)`)
#'  Whether the deletion has to be confirmed interactively.
#'
#' @export
delete = function(type, id, api_key = NULL, server = NULL, confirm = TRUE) {
  if (is.null(api_key)) api_key = get_api_key()
  if (is.null(server)) server = get_server()

  if (confirm) {
    ask_confirmation("delete")
  }
  assert_choice(type, choices = c("flow", "run", "task", "data", "study", "collection"))
  if (type == "collection") type = "study"
  url = sprintf("%s/%s/%s", server, type, id)
  response = httr::DELETE(url, query = list(api_key = api_key))
  response
}


#' @title Returns the currently active server
#'
#' @description Returns the server that can be configures via the option `mlr3oml.server`.
#' Returns "https://www.openml.org/api/v1" by default.
#'
#' @export
get_server = function() {
  stopf("Don't use get_server anymore")
  server = getOption("mlr3oml.server", "https://openml.org/api/v1")
  assert_choice(server, c("https://openml.org/api/v1", "https://test.openml.org/api/v1"))
  server
}

# extracts `flow_exists` from the response
# is -1 if it does not exist and returns the id otherwise
id_from_flow_response = function(response) {
  # TODO: write this file for different response type (run, ... ) and use it in publish.R
  id = as.integer(xml2::as_list(httr::content(response))$flow_exists$id[[1]])
  if (id == -1L) {
    return(NULL)
  }
  return(id)
}

ask_confirmation = function(action = "publish") {
  user_input = readline(sprintf("Are you sure you want to %s on OpenML? (y/n)  ", action))
  if (user_input != "y") stop("Exiting since you did not press y.")
}

get_data_from_backend = function(backend) {
  ii = backend$data(backend$data(backend$primary_key))
}

