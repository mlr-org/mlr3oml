#' @rdname list_oml
#' @param uploader (`integer(1)`)\cr
#'   Filter for uploader.
#' @export
list_oml_flows = function(uploader = NULL, tag = NULL, limit = limit_default(),
  test_server = test_server_default(), ...) {
  tab = get_paginated_table("flow",
    uploader = uploader,
    tag = tag,
    limit = limit,
    server = get_server(test_server),
    ...
  )

  if (nrow(tab)) {
    setnames(tab, "id", "flow_id")
  }

  return(tab[])
}
