#' @export
publish.Learner = function(x, ...) {
  # 1. Assertions
  # if (flow_exists(x)) stop("Flow already exists on OpenML")
  # TODO: implement this, for that we need a good external version
  api_key = get_api_key()
  if (is.na(api_key)) stop("API key required for upload.")
  assert_character(api_key, any.missing = FALSE, len = 1)


  # 2. Create the data for upload
  description = make_description(x)
  server = getOption("mlr3oml.server") %??% "https://www.openml.org/api/v1"
  url = paste0(server, "/flow")
  file = tempfile(fileext = ".xml")
  withr::defer(unlink(file))
  xml2::write_xml(x = description, file = file)
  response = upload(url = url,
                    body = list(description = httr::upload_file(file)))
  flow_id = as.integer(xml2::as_list(response)$upload_flow$id[[1]])
  mlr3misc::messagef("Your flow was successfully uploaded and assigned id: %i.", flow_id)
  return(flow_id)
}


