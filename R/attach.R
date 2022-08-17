#' @title Attach to a Study
#' @description
#' Detaches an object from a study / collection.
#' @param study_id (`integer(1)`)\cr
#'   The id of the study.
#' @param ids (`integer()`)\cr
#'   The id of the object to attach to the study. Depending on the type of the study (it's main
#'   entity type), this is either a task id or run id.
#' @export
attach_to_study = function(study_id, ids) {
  url = sprintf("%s/study/%s/attach", get_server(), study_id)
  response = httr::POST(
    url = url,
    body = list(ids = as.character(ids)),
    query = list(api_key = mlr3oml:::get_api_key())
  )
  response = xml2::as_list(httr::content(response))
  return(response)
}

#' @title Detach from a Study
#' Detaches an object from a study / collection.
#' @param study_id (`integer(1)`)\cr
#'   The id of the study.
#' @param ids (`integer()`)\cr
#'   The id of the object to attach to the study. Depending on the type of the study (it's main
#'   entity type), this is either a task id or run id.
#' @export
detach_from_study = function(study_id, ids) {
  url = sprintf("%s/study/%s/detach", get_server(), study_id)
  response = httr::POST(
    url = url,
    body = list(ids = as.character(ids)),
    query = list(api_key = mlr3oml:::get_api_key())
  )
  response = xml2::as_list(httr::content(response))
  return(response)
}
