download_data = function(data_id, desc = download_data_desc(data_id), parquet) {
  if (parquet) {
    require_namespaces("duckdb")
    library(mlr3oml)
    library(data.table)
    library(duckdb)
    id = 31
    odata = OMLData$new(id)
    odata$data
    url = odata$desc$minio_url
    path = tempfile()
    download.file(url, path)
    db = dbConnect(duckdb::duckdb())
    data = dbGetQuery(db, sprintf("SELECT * FROM read_parquet(['%s']);", path))
    data = as.data.table(data)

  }

  data = get_arff(desc$url, sparse = (desc$format == "sparse_arff"))
  return(data)
}
