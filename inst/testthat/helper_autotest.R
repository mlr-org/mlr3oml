#' @title Autotest for flows
#' @description Does various tests regarding upload, download and conversion of tests.
#' @details The following (can be) tested:
#' - Can flows be uploaded to the server.
#' - Can flows be downloaded from the server.
#' - Can flows be converted to mlr3 learners.
#'   For that locally stored files are used.
#'
autotest_flow = function(n_download = 10, n_upload=0, download = FALSE,
                         upload = FALSE, local = TRUE) {
  # First the test with the test server
  with_test_server()
  if (download && curl::has_internet() && n_download > 0) {
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

  # Then the tests with the local data

  #if (local) {
  #  desc_list = system.file("inst/testdata", package = "mlr3extralearners")
  #  for (desc in desc_list) {
  #    pint(

  #  }
  #}


}


pseudo_publish = function(x) {
  desc = make_description(x)
  x
}

make_test_flows = function() {
  path = system.file("inst/testdata", package = "mlr3oml")
  # first the standard learners
  ll = mlr3extralearners::list_mlr3learners(select = c("id", "class"))
  lids = ll[class %in% c("regr", "class"), ]$id
  lids = lids[c(1, 4, 7, 15)]
  for (lid in lids) {
    learner = lrn(lid)
    response = publish(learner)
  }

  # then a
}



