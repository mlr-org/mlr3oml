#' @export
get_id = function(x, ...) {
  UseMethod("get_id")
}

#' @export
get_id.Learner = function(x, ...) {
  get_private(x)$oml$id
}

#' @export
get_id.Task = function(x, ...) {
  private = get_private(x)
  oml_id = private$oml$id
  if (is.null(oml_id)) {
    return(NULL)
  }
  current_hash = calculate_hash(
    class(x), x$id, x$backend$hash, x$col_info,
    private$.row_roles, private$.col_roles, private$.properties
  )
  if (private$oml$hash == current_hash) {
    return(oml_id)
  }
  messagef("Task originally from OpenML but has since been modified, returning NULL.")
  return(NULL)
}

#' @export
get_id.Resampling = function(x, ...) {
  oml_id = get_private(x)$oml$id
  if (is.null(oml_id)) {
    return(NULL)
  }
  current_hash = calculate_hash(
    list(class(x), x$id, x$param_set$values, x$instance)
  )
  if (get_private(x)$oml$hash == current_hash) {
    return(oml_id)
  }
  messagef("Resampling originally from OpenML but has since been modified, returning NULL.")
  return(NULL)
}
