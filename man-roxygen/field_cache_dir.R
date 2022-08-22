#' @field cache_dir (`logical(1)` | `character(1)`)\cr
#' Stores the location of the cache for objects retrieved from OpenML.
#' If set to `FALSE`, caching is disabled.
#' Objects from the test server are stored in the subdirectory 'test', those from the public
#' server are stored in the subdirectory 'public'.
#'
#' The package \CRANpkg{qs} is required for caching.
