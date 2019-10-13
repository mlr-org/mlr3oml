#' @import checkmate
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("mlr3oml", libpath)
}
