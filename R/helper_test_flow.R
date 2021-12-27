#' Tests for OML Flows
#'
#' @description Does various tests regarding upload, download and conversion of tests.
#' Some tests require the server, while others are simulated locally.
#' @details The following can tested:
#' - Can flows be uploaded to the test server.
#' - Can flows be downloaded from the server.
#' - Can flows be converted to mlr3 learners.
#'   For that locally stored files are used.
#'


#' This writes test flows to ./inst/testdata that can be used threshold for local testing
make_test_flows = function() {
  path = system.file("inst/testdata", package = "mlr3oml")
  # first the standard learners
  lrn = lrn("regr.bart")
  desc = make_description(desc)
  # The OpenML Server adds various info such as the external_version, id, upload_id, time
  # They have to be inserted at the right position

  # then a
}

#' Loads the flows that were created with `pseudo_publish_learner` from disk into R.
pseudo_get_flow = function(name) {
  path = system.file("inst/testdata", package = "mlr3oml")
  desc = jsonlite::fromJSON(sprintf("%s/%s", path, name))$flow
  desc = parse_flow_desc(desc)
  flow = OMLFlow$new(1)
  # overwrite description before it is downloaded (is done lazily)
  flow$.__enclos_env__$private$.desc = desc
  return(flow)
}



#' Creates the xml descriptin and writes it to the disk for testing purposes.
pseudo_publish_learner = function(x) {
  path = system.file("inst/testdata", package = "mlr3oml")
  desc = make_description(x)
  flow = xml2::xml_root(desc)
  xml2::xml_add_child(flow, "id", 314, .where = 1)
  xml2::xml_add_child(flow, "uploader", 271, .where = 2)
  xml2::xml_add_child(flow, "version", 1, .where = 4)
  xml2::xml_add_child(flow, "external_version", 10101, .where = 5)
  # to make the format the same as returned from OpenML
  time = paste(strsplit(as.character(Sys.time()), " ")[[1]], collapse = "T")
  xml2::xml_add_child(flow, "upload_date", time, .where = 7)

  desc_list = xml2::as_list(flow)
  names(desc_list) = "flow"
  desc_list = unlist_leaves(desc_list)
  desc_list$flow[grepl("parameter", names(desc_list$flow))] = NULL
  params = as.data.table(x$param_set)
  params = params[, list(id, class, default)]
  names(params) = c("name", "data_type", "default_value")
  params$default_value = mlr3misc::map(params[, default_value], format_default)
  desc_list$flow$parameter = params
  attributes(desc_list$flow$upload_date) = NULL
  jsonlite::write_json(desc_list, paste0(path, "/", x$id))
  return(desc_list)
}

#' Helper function when creating the list from the xml-description
#' When applied to list(a = list(1), b = list(c = list(2))) it returns
#' list(a = 1, b = list(2))
unlist_leaves = function(l, i = 1) {
  for (j in seq_len(length(l))) {
    if (!is.list(l[[j]])) {
      return(l[[j]])
    } else {
      l[[j]] = unlist_leaves(l[[j]], i = 1)
    }
  }
  return(l)
}
