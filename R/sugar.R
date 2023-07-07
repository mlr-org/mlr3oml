#' @title Syntactic Sugar for Data Construction
#'
#' @description
#' Creates an [`OMLData`] instance.
#'
#' @template param_id
#' @template param_test_server
#' @template param_parquet
#'
#' @return ([`OMLData`])
#'
#' @export
odt = function(
  id,
  parquet = parquet_default(),
  test_server = test_server_default()
  ) {
  OMLData$new(id = id, parquet = parquet, test_server = test_server)
}

#' @title Syntactic Sugar for Task Construction
#'
#' @description
#' Creates an [`OMLTask`] instance.
#'
#' @template param_id
#' @template param_test_server
#' @template param_parquet
#'
#' @return ([`OMLTask`])
#'
#' @export
otsk = function(
  id,
  parquet = parquet_default(),
  test_server = test_server_default()
  ) {
  OMLTask$new(id = id, parquet = parquet, test_server = test_server)
}

#' @title Syntactic Sugar for Flow Construction
#'
#' @description
#' Creates an [`OMLFlow`] instance.
#'
#' @template param_id
#' @template param_test_server
#'
#' @return ([`OMLFlow`])
#'
#' @export
oflw = function(
  id,
  test_server = test_server_default()
  ) {
  OMLFlow$new(id = id, test_server = test_server)
}

#' @title Syntactic Sugar for Run Construction
#'
#' @description
#' Creates an [`OMLRun`] instance.
#'
#' @template param_id
#' @template param_test_server
#' @template param_parquet
#'
#' @return ([`OMLRun`])
#'
#' @export
orn = function(
  id,
  parquet = parquet_default(),
  test_server = test_server_default()
  ) {
  OMLRun$new(id = id, parquet = parquet, test_server = test_server)
}

#' @title Syntactic Sugar for Collection Construction
#'
#' @description
#' Creates an [`OMLCollection`] instance.
#'
#' @template param_id
#' @template param_test_server
#'
#' @return ([`OMLCollection`])
#'
#' @export
ocl = function(
  id,
  test_server = test_server_default()
  ) {
  OMLCollection$new(id = id, test_server = test_server)
}

