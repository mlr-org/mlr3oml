#' @rdname list_oml
#' @param main_entity_type (`character()`)\cr
#'   The main entity type.
#' @param uploader (`integer()`)\cr
#'   The uploader.
#' @export
list_oml_collections = function(main_entity_type = NULL, uploader = NULL,
  limit = getOption("mlr3oml.limit", 5000L), test_server = getOption("mlr3oml.test_server", FALSE), ...) {
  tab = get_paginated_table("study",
    main_entity_type = main_entity_type,
    uploader = uploader,
    limit = limit,
    server = get_server(test_server),
    ...
  )

  if (nrow(tab)) {
    set(tab, j = "id", value = as.integer(tab$id))
    set(tab, j = "creator", value = as.integer(tab$creator))
    set(tab, j = "creation_date", value = NULL)
  }

  return(tab)
}
