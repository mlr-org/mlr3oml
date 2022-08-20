task_type_translator = function(tt, to = "mlr3") {
  if (to == "mlr3") {
    converted = switch(tt,
      "Supervised Regression" = "regr",
      "Supervised Classification" = "classif",
      "Survival Analysis" = "surv",
      "Clustering" = "clust",
      NULL
    )
  }
  if (to == "oml") {
    converted = switch(tt,
      "regr" = "Supervised Regression",
      "classif" = "Supervised Classification",
      "surv" = "Survival Analysis",
      "clust" = "Clustering",
      NULL
    )
  }
  return(converted)
}

# When uploading predictions to OpenML, they have to satisfy a specific format.
# This function takes a mlr3 Prediction and converts it into that format
# TODO: check whether the -1L is really necessary for the fold_ids when the test server
# is up and running again
# Every OpenML prediction is a data.table with:
# repeat | fold | row_id | prediction | truth
# For classification tasks there are additional columns
# confidence.class1 | confidence.class2 | ...
# In mlr3 these are set to the probabilities if they are available and otherwise to 1 for the
# predicted class and 0 for everything else.
make_oml_prediction = function(rr) {
  resampling = rr$resampling
  prediction = rr$prediction()
  learner = rr$learner

  if (test_r6(rr$resampling, "ResamplingCV")) {
    prediction_oml = data.table(
      "repeat" = 0L, # repeat is a keyword in R
      fold = resampling$instance$fold - 1L,
      row_id = prediction$row_ids - 1L,
      prediction = prediction$response,
      truth = prediction$truth
    )
  } else if (test_r6(resampling, "ResamplingRepeatedCV")) {
    prediction_oml = data.table(
      "repeat" = resampling$instance$rep - 1L,
      fold = prediction$instance$fold - 1L,
      row_id = prediction$row_ids - 1L,
      prediction = prediction$response,
      truth = prediction$truth
    )
  } else if (test_r6(resampling, "ResamplingLOO")) {
    prediction_oml = data.table(
      "repeat" = 0L, # repeat is a keyword in R
      fold = seq(0L, nrow(prediction) - 1L),
      row_id = prediction$row_ids - 1L,
      prediction = prediction$response,
      truth = prediction$truth
    )
  } else if (test_r6(resampling, "ResamplingHoldout")) {
    prediction_oml = data.table(
      "repeat" = 0L, # repeat is a keyword in R
      fold = 0L,
      row_id = prediction$row_ids - 1L,
      prediction = prediction$response,
      truth = prediction$truth
    )
  } else {
    stopf("Resampling of type %s not supported.", class(resampling)[[1L]])
  }

  # add the confidence
  if (learner$task_type == "classif") {
    if (learner$predict_type == "response") {
      levels = levels(prediction$truth)
      confidence = list()
      for (lv in levels) {
        confidence[[sprintf("confidence.%s", lv)]] = ifelse(lv == prediction$response, 1, 0)
      }
    } else if (learner$predict_type == "prob") {
      confidence = prediction$prob
      colnames(confidence) = paste("confidence", colnames(confidence), sep = ".")
    } else {
      stopf("Predict type %s not supported", learner$predict_type)
    }
    confidence = as.data.table(confidence)
    prediction_oml = cbind(prediction_oml, confidence)
    # colnames(prediction_oml)[colnames(prediction_oml) == "truth"] = "correct"
  }
  return(prediction_oml)
}


read_parquet = function(path) {
  require_namespaces(c("duckdb", "DBI"))
  con = DBI::dbConnect(duckdb::duckdb())
  data = DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s');", path))
  DBI::dbDisconnect(con, shutdown = TRUE)
  return(data)
}

get_desc_downloader = function(type) {
  switch(type,
    collection = download_desc_collection,
    study = download_desc_collection,
    run = download_desc_run,
    data = download_desc_data,
    task = download_desc_task,
    run = download_desc_run,
    flow = download_desc_flow,
    stopf("Invalid type '%s'.", type)
  )
}

get_server = function(test_server) {
  ifelse(test_server, "https://test.openml.org/api/v1", "https://www.openml.org/api/v1")
}
