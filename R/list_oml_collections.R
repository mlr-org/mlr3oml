#' @rdname list_oml
#' @param uploader (`integer(1)`)
#'   Filter for uploader.
#' @param status (`character(1)`)\cr
#'   Should be one of "active", "in_preparation", "deactivated", "all". By
#'   default "all" studies are returned.
#' @param main_entity_type (`character(1)` | `NULL`)\cr
#'   Filter for main entity type. Can be "run" or "task".
#' @export
list_oml_collections = function(uploader = NULL, status = "all", main_entity_type = NULL,
  limit = limit_default(), test_server = test_server_default(), ...) {

  assert_choice(status, c("active", "in_preparation", "deactivated", "all"))
  assert_choice(main_entity_type, c("run", "task"), null.ok = TRUE)
  uploader = assert_integerish(uploader, len = 1L, null.ok = TRUE, coerce = TRUE)

  tab = get_paginated_table("study",
    main_entity_type = main_entity_type,
    uploader = uploader,
    status = status,
    limit = limit,
    server = get_server(test_server),
    ...
  )

  if (nrow(tab)) {
    setnames(tab, "id", "collection_id")
  }

  return(tab[])
}
