# When names can be very long (e.g. flows) this function can be used for better formatting
truncate_name = function(name, width = 30L) {
  ifelse(nchar(name) < width, name, paste0(strtrim(name, width), "[...]"))
}

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
  }
  if (test_r6(resampling, "ResamplingRepeatedCV")) {
    prediction_oml = data.table(
      "repeat" = resampling$instance$rep - 1L,
      fold = prediction$instance$fold - 1L,
      row_id = prediction$row_ids - 1L,
      prediction = prediction$response,
      truth = prediction$truth
    )
  }
  if (test_r6(resampling, "ResamplingLOO")) {
    prediction_oml = data.table(
      "repeat" = 0L, # repeat is a keyword in R
      fold = seq(0L, nrow(prediction) - 1L),
      row_id = prediction$row_ids - 1L,
      prediction = prediction$response,
      truth = prediction$truth
    )
  }
  if (test_r6(resampling, "ResamplingHoldout")) {
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


# @title Check the dependencies of the flow and compare it with the installed versions
# @description
# Compares the dependencies of the flow with those installed.
# @param flow (`OMLFLow`) The flow whose dependencies are checked. Should only  be called on
# mlr3 Flows.
# @param versbose (`logical(1)`) Whether to be verbose.
#
# @return Returns TRUE if the dependencies match and FALSE otherwise
check_dependencies = function(flow, verbose) {
  dependencies = flow$dependencies
  is_R = startsWith(dependencies, "R_")
  R_version = dependencies[is_R]
  R_version_running = paste0("R_", paste0(R.Version()[c("major", "minor")], collapse = "."))

  if (length(R_version) && (R_version != R_version_running && verbose)) {
    messagef("Flow's R version (%s), differs from running R version (%s).", R_version,
      R_version_running
    )
  }

  dependencies = dependencies[!is_R]

  # deps_flow = c(mlr = "0.1.1", rpart = "0.2.3")
  pkgs_flow = map_chr(strsplit(dependencies, split = "_"), 1)
  versions_flow = map_chr(strsplit(dependencies, split = "_"), 2)
  deps_flow = set_names(versions_flow, pkgs_flow)

  installed_pkgs = installed.packages()
  installed_pkgs = installed_pkgs[rownames(installed_pkgs) %in% pkgs_flow, , drop = FALSE]
  installed_pkgs = paste(
    installed_pkgs[, "Package", drop = FALSE],
    installed_pkgs[, "Version", drop = FALSE],
    sep = "_"
  )

  if (!length(installed_pkgs)) {
    if (verbose) {
      messagef("Missing all required dependencies: %s", paste(dependencies, collapse = ", "))
    }
    return(FALSE)
  }

  pkgs_lrn = map_chr(strsplit(installed_pkgs, split = "_"), 1)
  versions_lrn = map_chr(strsplit(installed_pkgs, split = "_"), 2)
  deps_lrn = set_names(versions_lrn, pkgs_lrn)

  everything_correct = length(dependencies) == length(installed_pkgs) &&
    all(sort(dependencies) == sort(installed_pkgs)) && R_version == R_version_running

  if (everything_correct) {
    return(TRUE)
  }

  # now two cases: either all are correctly installed or not
  missing_pkgs = setdiff(pkgs_flow, pkgs_lrn)

  if (length(missing_pkgs) && verbose) {
    messagef(
      "Flow has has uninstalled dependencies: %s",
      paste(paste(missing_pkgs, deps_flow[missing_pkgs], sep = "_"), collapse = ", ")
    )
  }

  pkgs_with_conflicts = names(which(deps_lrn[pkgs_lrn] != deps_flow[pkgs_lrn]))
  warning = map(
    pkgs_with_conflicts,
    function(pkg) {
      paste0(" * ", pkg, ": ", deps_lrn[pkg], " != ", deps_flow[pkg])
    }
  )
  message = paste(
    paste0("Version conflicts (installed != required)\n", paste(warning, collapse = "\n")),
    "Due to these conflicts, new results using this learner cannot be published.", sep = "\n"
  )
  if (verbose) {
    messagef(message)

  }
  return(FALSE)
}
