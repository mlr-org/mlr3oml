#' @field cache (`logical(1)` | `character(1)`)\cr
#' Controls how objects retrieved from \url{https://openml.org} are cached on
#' the local file system.
#'
#' * `FALSE` to disable caching completely.
#' * `TRUE` to enable caching in path returned by [R_user_dir("mlr3oml", "cache")][R_user_dir()]
#' * A custom path as single string.
#'
#' The package \CRANpkg{qs} is required for caching.
