#' @title Gets the OpenML id of an mlr3 object (if it exists).
#' @description OpenML objects can be converted to mlr3 objects. After this conversion the id is
#' added to a private slot of the object. This S3 generic retries this id and checks whether the
#' object has been modified since conversion (by comparing the hashes). If it was modified
#' (hash changed), NULL is returned.
#'
#' @param x (`any`) The mlr3 object whose OpenML id is queried.
#' @param ... Currently unused.
#' @return
#' Id of the object.
#' @examples
#' \donttest{
#' library("mlr3")
#' otask = OMLTask$new(31)
#' task = as_task(otask)
#' # returns the id
#' get_id(task)
#' task$col_roles$features = "duration"
#' # returns NULL
#' get_id(task)
#' }
#'
#' @export
get_id = function(x, ...) {
  UseMethod("get_id")
}

#' @export
get_id.Learner = function(x, ...) {
  # TODO: we don't really need that?
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
