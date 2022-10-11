#' Syntactic Sugar for Object Construction
#'
#' Functions to create OpenML objects. The following functions are available:
#' * `oml_data()` - creates an instance of the R6 class [`OMLData`].
#' * `oml_task()` - creates an instance of the R6 class [`OMLTask`].
#' * `oml_flow()` - creates an instance of the R6 class [`OMLFlow`].
#' * `oml_run()` - creates an instance of the R6 class [`OMLRun`].
#' * `oml_collection()` - creates an instance of the R6 class [`OMLCollection`].
#'
#' @name oml_sugar
#' @template param_id
#' @template param_cache
#' @template param_test_server
#' @template param_parquet
#'
#' @return ([`OMLObject`])
#'
#' @export
oml_data = function(
  id,
  cache = cache_default(),
  parquet = parquet_default(),
  test_server = test_server_default()
  ) {
  OMLData$new(id = id, cache = cache, parquet = parquet, test_server = test_server)
}

#' @rdname oml_sugar
#' @export
oml_task = function(
  id,
  cache = cache_default(),
  parquet = parquet_default(),
  test_server = test_server_default()
  ) {
  OMLTask$new(id = id, cache = cache, parquet = parquet, test_server = test_server)
}

#' @rdname oml_sugar
#' @export
oml_flow = function(
  id,
  cache = cache_default(),
  test_server = test_server_default()
  ) {
  OMLFlow$new(id = id, cache = cache, test_server = test_server)
}

#' @rdname oml_sugar
#' @export
oml_run = function(
  id,
  cache = cache_default(),
  parquet = parquet_default(),
  test_server = test_server_default()
  ) {
  OMLRun$new(id = id, cache = cache, parquet = parquet, test_server = test_server)
}

#' @rdname oml_sugar
#' @export
oml_collection = function(
  id,
  cache = cache_default(),
  parquet = parquet_default(),
  test_server = test_server_default()
  ) {
  OMLCollection$new(id = id, cache = cache, parquet = parquet, test_server = test_server)
}

