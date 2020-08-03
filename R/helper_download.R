download_file = function(url, path, status_ok = integer()) {
  lg$debug("Downloading to local file system", url = url, path = path)
  res = curl::curl_fetch_disk(url, path)
  status_code = res$status_code

  if (status_code %nin% c(200L, status_ok)) {
    msg = jsonlite::fromJSON(readLines(res$content, warn = FALSE))$error$message
    stopf("Error downloading '%s' (status code: %i, message: '%s')", url, status_code, msg)
  }

  return(res$status_code)
}


get_json = function(url, ..., simplifyVector = TRUE, simplifyDataFrame = TRUE, status_ok = integer()) {
  path = tempfile(fileext = ".json")
  on.exit(file.remove(path[file.exists(path)]))
  url = sprintf(url, ...)

  lg$info("Retrieving JSON", url = url)

  api_key = getOption("mlr3oml.api_key")
  if (!is.null(api_key)) {
    url = sprintf("%s?api_key=%s", url, api_key)
  }

  status = download_file(url, path, status_ok = status_ok)
  if (status != 200L) {
      return(NULL)
  }

  jsonlite::fromJSON(path, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame)
}


get_arff = function(url, ...) {
  path = tempfile(fileext = ".arff")
  on.exit(file.remove(path[file.exists(path)]))
  url = sprintf(url, ...)

  lg$info("Downloading ARFF", url = url)

  download_file(url, path)

  lg$debug("Start processing ARFF file", path = path)
  read_arff(path)
}
