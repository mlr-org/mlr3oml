#' @import utils
#' @import checkmate
#' @import data.table
#' @import R6
#' @import mlr3misc
#' @import mlr3
NULL

.onLoad = function(libname, pkgname) {
  backports::import(pkgname)
  mlr3::mlr_tasks$add("oml", OMLTaskConnector)
}

.onUnload <- function (libpath) {
  # library.dynam.unload("mlr3oml", libpath)
}
