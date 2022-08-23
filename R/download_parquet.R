download_parquet = function(data_id, server, desc = download_desc_data(data_id, server)) {
  file = tempfile(fileext = ".parquet")
  download.file(desc$minio_url, file)
  return(file)
}

