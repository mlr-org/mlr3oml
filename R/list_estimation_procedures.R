# #' @rdname list_oml
# #' @param uploader (`integer(1)`)\cr
# #'   Filter for uploader.
# #' @export
# list_oml_estimation_procedures = function(uploader = NULL, tag = NULL,
#   limit = getOption("mlr3oml.limit", 5000L), ...) {
#   tab = get_paginated_table("estimationprocedure",
#     uploader = uploader,
#     tag = tag,
#     limit = limit,
#     ...
#   )
#
#   if (nrow(tab)) {
#     setnames(tab, "id", "flow_id")
#   }
#
#   return(tab[])
# }
#
# if (FALSE) {
#   debugonce(list_oml_estimation_procedures)
#   list_oml_estimation_procedures()
# }
# https://www.openml.org/#!/estimationprocedure/get_estimationprocedure_list
