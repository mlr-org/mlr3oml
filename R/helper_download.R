download_file = function(url, path) {
  resp = curl::curl_fetch_disk(url, path)

  if (resp$status_code != 200L) {
    stopf("Failed to download '%s' (status code: %i)", resp$url, resp$status_code)
  }

  path
}

get_json = function(url, ..., simplifyVector = TRUE, simplifyDataFrame = TRUE) {
  path = tempfile(fileext = ".json")
  on.exit(file.remove(path[file.exists(path)]))
  download_file(sprintf(url, ...), path = path)

  jsonlite::fromJSON(path, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame)
}


get_arff = function(url, ...) {
  path = tempfile(fileext = ".arff")
  on.exit(file.remove(path[file.exists(path)]))
  download_file(sprintf(url, ...), path = path)

  read_arff(path)
}
