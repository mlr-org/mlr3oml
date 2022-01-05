#' @title Autotest for flows
#' @description Does various tests regarding upload, download and conversion of tests.
#' Some tests require the server, while others are simulated locally.
#' @details The following can tested:
#' - Can flows be uploaded to the test server.
#' - Can flows be downloaded from the server.
#' - Can flows be converted to mlr3 learners.
#'   For that locally stored files are used.
#'

autotest_flow = function(n_upload = 10, n_download = 0, n_local = 10) {

  # First the test with the test server
  with_test_server()
  if (n_upload) {
    flow_ids = list_oml_flows(limit = 100000L)$flow_id
    flow_ids = sample(flow_ids, n)
    for (flow_id in flow_ids) {
      expect_flow(OMLFlow$new(flow_id))
    }
  }

  if (upload && has_internet() && n_upload > 0) {
    ll = mlr3extralearners::list_mlr3learners(select = c("id", "class"))
    ll = ll[class %in% c("regr", "class"), ]
    lids = ll$id
    for (lid in lids) {
      lrn = lrn(id)
      flow_id = publish(lrn)
      flow = OMLFlow$new(flow_id)
      all.equal(flow$convert(), lrn)
    }
  }
}

download_error_flow = function(id) {
  paste0(
    "Error : Error downloading 'https://openml.org/api/v1/json/flow/", id,
    "' (http code: 412, oml code: 181, message: 'Unknown flow'\n"
  )
}



#' @title autotest_flow_download
#' @description Tests whether random flows can be downloaded from OpenML
#' @details The flow ids that are tested are sampled randomly. There are then two cases;:
#'  1. A flow with the id exists --> Has to satisfy expect_flow
#'  2. A flow with the id does not exist --> We expect the correct 'download error'
#' This is done as long as either max_tries tries are exhausted or n flows that exist were found.
autotest_flow_download = function(n_flows, max_tries = n_flows * 2) {
  with_public_server()

  if (n_flows) {
    flow_ids = sample(13000L, max_tries)
    total_downloads = 0
    i = 1
    while (total_downloads < n_flows && i <= max_tries) {
      flow = OMLFlow$new(flow_ids[i])
      error_capture = suppressMessages(try(flow$desc, silent = TRUE))
      if (class(error_capture) == "try-error") {
        expect_equal(error_capture[1], download_error(flow_ids[i]))
      } else {
        expect_flow(flow)
        total_downloads = total_downloads + 1
      }
      print(i)
      i = i + 1
    }
  }
}


pseudo_publish = function(x) {
  desc = make_description(x)
  x
}

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

pseudo_get_flow = function(name) {
  path = system.file("inst/testdata", package = "mlr3oml")
  desc = jsonlite::fromJSON(sprintf("%s/%s", path, name))
  desc = parse_flow_desc(desc)
  # this is a bit hacky, we make use of the lazy downloading and before the description for id
  # 1 is downloaded we overwrite the private field .desc
  flow = OMLFlow$new(1)
  flow$.__enclos_env__$private$.desc = desc
  return(flow)
}



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
  desc_list = xml_helper(desc_list)
  desc_list$flow[grepl("parameter", names(desc_list$flow))] = NULL
  params = as.data.table(x$param_set)
  params = params[, list(id, class, default)]
  names(params) = c("name", "data_type", "default")
  params$default = mlr3misc::map(params[, default], format_default)
  desc_list$flow$parameter = params
  attributes(desc_list$flow$upload_date) = NULL
  jsonlite::write_json(desc_list, paste0(path, "/", x$id))
  return(desc_list)
}







xml_helper = function(l, i = 1) {
  for (j in seq_len(length(l))) {
    if (!is.list(l[[j]])) {
      return(l[[j]])
    } else {
      l[[j]] = f(l[[j]], i = 1)
    }
  }
  return(l)
}
