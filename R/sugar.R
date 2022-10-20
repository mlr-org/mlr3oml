#' Syntactic Sugar for Object Construction
#'
#' Functions to create OpenML objects. The following functions are available:
#' * `odt()` - creates an instance of the R6 class [`OMLData`].
#' * `otsk()` - creates an instance of the R6 class [`OMLTask`].
#' * `oflw()` - creates an instance of the R6 class [`OMLFlow`].
#' * `orn()` - creates an instance of the R6 class [`OMLRun`].
#' * `ocl()` - creates an instance of the R6 class [`OMLCollection`].
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
odt = function(
  id,
  cache = cache_default(),
  parquet = parquet_default(),
  test_server = test_server_default()
  ) {
  OMLData$new(id = id, cache = cache, parquet = parquet, test_server = test_server)
}

#' @rdname oml_sugar
#' @export
otsk = function(
  id,
  cache = cache_default(),
  parquet = parquet_default(),
  test_server = test_server_default()
  ) {
  OMLTask$new(id = id, cache = cache, parquet = parquet, test_server = test_server)
}

#' @rdname oml_sugar
#' @export
oflw = function(
  id,
  cache = cache_default(),
  test_server = test_server_default()
  ) {
  OMLFlow$new(id = id, cache = cache, test_server = test_server)
}

#' @rdname oml_sugar
#' @export
orn = function(
  id,
  cache = cache_default(),
  parquet = parquet_default(),
  test_server = test_server_default()
  ) {
  OMLRun$new(id = id, cache = cache, parquet = parquet, test_server = test_server)
}

#' @rdname oml_sugar
#' @export
ocl = function(
  id,
  cache = cache_default(),
  parquet = parquet_default(),
  test_server = test_server_default()
  ) {
  OMLCollection$new(id = id, cache = cache, parquet = parquet, test_server = test_server)
}

