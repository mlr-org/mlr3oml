#' @import checkmate
#' @import data.table
#' @import R6
#' @import mlr3misc
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("mlr3oml", libpath)
}
