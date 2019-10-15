#' @import checkmate
#' @import data.table
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("mlr3oml", libpath)
}
