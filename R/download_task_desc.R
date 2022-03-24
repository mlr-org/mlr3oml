download_task_desc = function(task_id) {
  server = get_server()
  desc = get_json(paste0(server, "/json/task/%i"), task_id,
    simplify_data_frame = FALSE
  )[[1L]]

  desc = parse_task_desc(desc)
  return(desc)
}

parse_task_desc = function(desc) {
  desc$task_id = as.integer(desc$task_id)
  desc$task_type_id = as.integer(desc$task_type_id)
  desc$input = set_names(map(desc$input, function(x) x[[2L]]), map_chr(desc$input, "name"))
  desc$input$source_data$data_set_id = as.integer(desc$input$source_data$data_set_id)
  est_params = desc$input$estimation_procedure$parameter
  for (i in seq(length(est_params))) {
    if (is.null(est_params[[i]]$value)) {
      est_params[[i]]$value = NA
    } else {
      name = est_params[[i]]$name
      if (name == "stratified_sampling") {
        est_params[[i]]$value = switch(est_params[[i]]$value,
          true = TRUE,
          false = FALSE
        )
      } else {
        est_params[[i]]$value = as.integer(est_params[[i]]$value)
      }
    }
  }
  desc$input$estimation_procedure$parameter = rbindlist(
    mlr3misc::map(est_params, function(x) as.data.table(list(name = x[[1]], value = list(x[[2]]))))
  )
  desc$input$estimation_procedure$id = as.integer(desc$input$estimation_procedure$id)

  desc$output = NULL

  return(desc)
}
