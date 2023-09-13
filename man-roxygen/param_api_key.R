#' @param api_key (`character(1)`)
#'  The API key to perform the action, if left NULL it first tries the "mlr3oml.api_key" R option and
#'  then the environment variable `OPENMLAPIKEY`.
#'
#'  In case `test_server` is TRUE (only relevant for developers) the test server API key is used, i.e. first the option
#'  "mlr3oml.test_api_key" and then the environment variable `TESTOPENMLAPIKEY`.
