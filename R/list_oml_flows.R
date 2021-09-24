#' @rdname list_oml
#' @param uploader (`integer(1)`)\cr
#'   Filter for uploader.
#' @export
list_oml_flows = function(uploader = NULL, tag = NULL, limit = getOption("mlr3oml.limit", 5000L), ...) {
  tab = get_paginated_table("flow",
    uploader = uploader,
    tag = tag,
    limit = limit,
    ...
  )

  if (nrow(tab)) {
    setnames(tab, "id", "flow_id")
  }

  return(tab[])
}
