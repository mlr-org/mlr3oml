#' @import checkmate
#' @import data.table
#' @import mlr3misc
#' @importFrom mlr3 as_data_backend TaskClassif TaskRegr
#' @importFrom R6 R6Class
#' @importFrom utils download.file
#'
#' @section Options:
#' * `mlr3oml.cache`: Enables or disables caching globally.
#'   If set to `FALSE`, caching is disabled.
#'   If set to `TRUE`, cache directory as reported by [R_user_dir()] is used.
#'   Alternatively, you can specify a path on the local file system here.
#'   Default is `FALSE`.
#' * `mlr3oml.api_key`: API key to use.
#'
#' @section Logging:
#' The \CRANpkg{lgr} package is used for logging.
#' To change the threshold, use `lgr::get_logger("mlr3oml")$set_threshold()`.
"_PACKAGE"

.onLoad = function(libname, pkgname) { # nocov start
  backports::import(pkgname)
  mlr3::mlr_tasks$add("oml", OMLTaskConnector)
  mlr3::mlr_resamplings$add("oml", OMLResamplingConnector)

  # setup logger
  assign("lg", lgr::get_logger(pkgname), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end

.onUnload <- function (libpath) { # nocov start
  library.dynam.unload("mlr3oml", libpath)
} # nocov end
