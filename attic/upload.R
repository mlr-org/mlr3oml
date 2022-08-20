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

