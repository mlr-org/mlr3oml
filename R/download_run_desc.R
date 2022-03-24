download_run_desc = function(run_id) {
  server = get_server()
  desc = get_json(paste0(server, "/json/run/%i"), run_id)[[1L]]
  desc = parse_run_desc(desc)
  return(desc)
}



parse_run_desc = function(desc) {
  # assert(desc$task_type %in% c("Supervised Regression", "Supervised Classification"))

  # Types get lost with the provided API --> convert character to ints
  desc$run_id = as.integer(desc$run_id)
  desc$input_data$dataset$did = as.integer(desc$input_data$dataset$did)
  desc$uploader = as.integer(desc$uploader)
  desc$task_id = as.integer(desc$task_id)
  desc$flow_id = as.integer(desc$flow_id)
  desc$setup_id = as.integer(desc$flow_id)

  # Output data is converted from dataframes to datatables

  # file: | did | file_id | name | url |
  desc$output_data$file = as.data.table(desc$output_data$file)

  # evaluation: | name | value | array_data | repeat | fold |
  if (is.null(desc$output_data$evaluation)) {
    desc$output_data$evaluation = data.table(
      "name" = character(0),
      "value" = numeric(0),
      "array_data" = list(),
      "rep" = integer(0),
      "fold" = integer(0)
    )
  } else {
    # In R it is ugly to work with `repeat` because it is a key word
    names(desc$output_data$evaluation)[names(desc$output_data$evaluation) == "repeat"] = "rep"
    desc$output_data$evaluation = as.data.table(desc$output_data$evaluation)

    # The contents of the file are stored as strings again that correspond (mostly) to json
    # format and therefore have to be parsed again
    if ("array_data" %in% colnames(desc$output_data$evaluation)) {
      desc$output_data$evaluation[, array_data := map(array_data, .f = parse_json_safely)]
    }
    desc$output_data$evaluation[, value := as.numeric(value)]
    if ("rep" %in% colnames(desc$output_data$evaluation)) {
      desc$output_data$evaluation[, rep := as.integer(rep)] # nolint
    }
    if ("fold" %in% colnames(desc$output_data$evaluation)) {
      desc$output_data$evaluation[, fold := as.integer(fold)] # nolint
    }
  }
  # Now the parameters: name | value | component

  desc$parameter_setting = as.data.table(desc$parameter_setting)
  desc$para
  # if (length(desc$parameter_setting)) {
  #  desc$parameter_setting = map(desc$parameter_setting, parse_json_safely)
  # }
  if (nrow(desc$parameter_setting)) {
    desc$parameter_setting[, value := map(value, .f = parse_json_safely)]
    desc$parameter_setting[["component"]] = as.integer(desc$parameter_setting[["component"]])
  }
  desc$parameter_setting[["name"]] = make.names(desc$parameter_setting[["name"]])
  return(desc)
}

parse_json_safely = function(x) {
  x = tryCatch(jsonlite::fromJSON(x),
    error = function(cond) {
      x
    }
  )
}
