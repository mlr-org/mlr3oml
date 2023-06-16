#' @rdname list_oml
#' @param flow_id (`integer(1)`)\cr
#'   Filter for flow id.
#' @param setup_id (`integer()`)\cr
#'   Vector of setup ids to restrict to.
#' @export
list_oml_setups = function(flow_id = NULL, setup_id = NULL, tag = NULL, limit = limit_default(),
  test_server = test_server_default(), ...) {
  tab = get_paginated_table("setup",
    flow = flow_id,
    setup = setup_id,
    tag = tag,
    limit = limit,
    server = get_server(test_server),
    ...
  )

  if (nrow(tab)) {
    tab[, c("setup_id", "flow_id") := list(
      as.integer(get("setup_id")),
      as.integer(get("flow_id"))
    )][]
  }

  return(tab[])
}
