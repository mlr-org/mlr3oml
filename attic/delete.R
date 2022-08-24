#' @title Delete an object from OpenML
#' @description This function can be used to delete objects from the OpenML server.
#'
#' @param type (`character(1)`)
#'  What type of object should be deleted ("flow", "run", "task", "data", "study" / "collection")
#' @param id (`integer(1)`)
#'  The id of the object that will be deleted.
#' @param api_key (`character(1)`)
#'  The API key to perform the action, if left NULL it first tries getOption("mlr3oml.api_key") and
#'  then Sys.getenv("OMLAPIKEY").
#' @param server (`character(1)`)
#'  The server address, defaults to `get_server()`.
#' @param confirm (`logical(1)`)
#'  Whether the deletion has to be confirmed interactively.
#'
#' @export
delete = function(type, id, api_key = NULL, server = NULL, confirm = TRUE) {
  if (is.null(api_key)) api_key = get_api_key()
  if (is.null(server)) server = get_server()

  if (confirm) {
    ask_confirmation("delete")
  }
  assert_choice(type, choices = c("flow", "run", "task", "data", "study", "collection"))
  if (type == "collection") type = "study"
  url = sprintf("%s/%s/%s", server, type, id)
  response = httr::DELETE(url, query = list(api_key = api_key))
  response
}
