download_file = function(url, path, quiet = TRUE) {
  lg$debug("Downloading to local file system", url = url, path = path)
  download.file(url, path, quiet = quiet)

  # resp = curl::curl_fetch_disk(url, path)
  # if (resp$status_code != 200L) {
  #   stopf("Failed to download '%s' (status code: %i)", resp$url, resp$status_code)
  # }
}

get_json = function(url, ..., simplifyVector = TRUE, simplifyDataFrame = TRUE) {
  path = tempfile(fileext = ".json")
  on.exit(file.remove(path[file.exists(path)]))
  url = sprintf(url, ...)

  lg$info("Retrieving JSON", url = url)

  api_key = getOption("mlr3oml.api_key")
  if (!is.null(api_key)) {
    url = sprintf("%s?api_key=%s", url, api_key)
  }
  download_file(url, path)

  jsonlite::fromJSON(path, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame)
}


get_arff = function(url, ...) {
  path = tempfile(fileext = ".arff")
  on.exit(file.remove(path[file.exists(path)]))
  url = sprintf(url, ...)

  lg$info("Downloading ARFF", url = url)

  download_file(url, path, quiet = FALSE)

  lg$debug("Start processing ARFF file", path = path)
  read_arff(path)
}
