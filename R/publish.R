#' @title Publish on OpenML
#'
#' @description
#' Publishes a {Flow, Run, Dataset, Task} to OpenML.
#' @details
#' This is a generic function.
#' @export

publish = function(x, ...) {
  UseMethod("publish", x)
}

publish.default = function(x, ...) {
  stop(sprintf("Objects of type %s cannot be published.", class(x)[[1]]))
}



