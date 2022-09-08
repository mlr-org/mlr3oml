download_desc_task = function(task_id, server) {
  desc = get_json(paste0(server, "/json/task/%i"), task_id,
    simplify_data_frame = FALSE, server = server
  )[[1L]]

  desc = parse_desc_task(desc)
  return(desc)
}

parse_desc_task = function(desc) {
  desc$task_id = as.integer(desc$task_id)
  desc$task_type_id = as.integer(desc$task_type_id)
  desc$input = set_names(map(desc$input, function(x) x[[2L]]), map_chr(desc$input, "name"))
  desc$input$source_data$data_set_id = as.integer(desc$input$source_data$data_set_id)
  est_params = desc$input$estimation_procedure$parameter
  ep_names = map(est_params, "name")
  ep_values = map(est_params, "value")

  ep_values = map(
    ep_values,
    function(p) {
      if (is.null(p)) {
        NA
      } else if (isTRUE(p == "true")) {
        TRUE
      } else if (isTRUE(p == "false")) {
        FALSE
      } else {
        as.integer(p)
      }
    }
  )
  desc$input$estimation_procedure$parameter = data.table(name = ep_names, value = ep_values)
  desc$input$estimation_procedure$id = as.integer(desc$input$estimation_procedure$id)

  return(desc)
}
