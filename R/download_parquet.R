# this is a cached function
download_parquet = function(url, id, cache_dir = FALSE, api_key = get_api_key(),
  retries = 3L) {
  lg$info("Retrieving parquet", url = url, authenticated = !is.na(api_key))
  type = "data_pq"


  if (isTRUE(cache_dir)) {
    assert_true(is.null(path))
    path = file.path(cache_dir, type, sprintf("%s.pq", id))

    if (file.exists(file)) {
      lg$debug("Object exists in cache.", type = type)
      return(path)
    } else {
      lg$debug("Storing object in cache.", type = type, id = id, file = file)
    }
  } else {
    path = tempfile(fileext = ".parquet")
  }

  for (retry in seq_len(retries)) {
    response = download_file(url, path, api_key = api_key)

    if (response$ok) {
      lg$debug("Downloaded parquet file", path = path)
      return(path)
    }
  }
  download_error(response)
}

