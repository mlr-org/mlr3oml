#' @title Autotest for flows
#' @description Does various tests regarding upload, download and conversion of tests.
#' @details The following can tested:
#' - Can flows be uploaded to the test server.
#' - Can flows be downloaded from the server.
#' - Can flows be converted to mlr3 learners.
#'   For that locally stored files are used.
#'



#' Helper function when converting xml-document into a list
#' Turns list(a = list(1), b = list(c = list(2))) into list(a = 1, b = list(c = 2))
unlist_leafs = function(l, i = 1) {
  for (j in seq_len(length(l))) {
    if (!is.list(l[[j]])) {
      return(l[[j]])
    } else {
      l[[j]] = unlist_leafs(l[[j]], i = 1)
    }
  }
  return(l)
}


##' @title make_download_test
##' @description Function factory that can create test functions for various OpenML objects.
##' @details This function creates a test object, that samples random ids and then tests
##' whether the downloaded OML objects satisfy the conditions defined in `expect_obj()`.
##' Because the sampled ids might not be existing OML objects, where is an additional
##' `download_error(id)` function, that takes as input the id and returns a `character(1)` that
##' contains the test message that is expected when the corresponding id does not exist on OpenML.
##' This error message has to be matched perfectly, otherwise the test fails.
##'
##' @details The flow ids that are tested are sampled randomly. There are then two cases;:
##'  1. A flow with the id exists --> Has to satisfy expect_flow
##'  2. A flow with the id does not exist --> We expect the correct 'download error'
##' This is done as long as either max_tries tries are exhausted or n flows that exist were found.
# make_download_test = function(expect_obj, obj_class) {
#  # we load valid ids from inst/testdata from which we then sample
#  type = switch(as.character(substitute(obj_class)),
#    "OMLData" = "data",
#    "OMLFlow" = "flow",
#    "OMLTask" = "task",
#    "OMLMeasure" = "measure",
#    "OMLResampling" = "resampling",
#    "OMLRun" = "run"
#  )
#  valid_ids = load_ids(type)
#
#  test_download_obj = function(n) {
#    with_public_server()
#    if (n) {
#      ids = sample(valid_ids, size = n)
#      for (i in seq_len(n)) {
#        id = ids[i]
#        expect_obj(obj_class$new(id))
#      }
#    }
#  }
# }


download_ids = function(type, limit = 100000L) {
  assert(type %in% c("task", "run", "evaluation", "setup", "data", "flow", "measure"))
  list_fn_name = switch(type,
    "data" = "list_oml_data",
    sprintf("list_oml_%ss", type)
  )
  path = system.file("inst", "testdata", package = "mlr3oml")
  ids = eval(call(list_fn_name, limit = limit))
  ids = ids[[1]]
  saveRDS(ids, sprintf("%s/%s_ids", path, type))
}

load_ids = function(type) {
  assert(type %in% c("task", "run", "evaluation", "setup", "data", "flow", "measure"))
  path = system.file("inst/testdata", package = "mlr3oml")
  ids = try(readRDS(sprintf("%s/%s_ids", path, type)), silent = TRUE)
  if (class(ids) == "try-error") {
    if (curl::has_internet()) {
      download_ids(type)
      ids = load_ids(type)
    } else {
      stop("Cannot load %s ids.", type)
    }
  }
  return(ids)
}
