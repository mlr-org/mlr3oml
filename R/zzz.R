#' @import checkmate
#' @import data.table
#' @import mlr3misc
#' @importFrom mlr3 as_data_backend TaskClassif TaskRegr
#' @importFrom R6 R6Class
#' @importFrom utils download.file
NULL

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
