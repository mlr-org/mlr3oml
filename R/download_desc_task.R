download_desc_task = function(task_id, server) {
  desc = get_json(paste0(server, "/tasks/%i"), task_id,
    simplify_data_frame = FALSE, server = server
  )

  desc = parse_desc_task(desc)
  return(desc)
}

parse_desc_task = function(desc) {
  desc$input = set_names(imap(desc$input, function(x, nm) x[[1]]), map_chr(desc$input, "name"))
  desc$output = set_names(imap(desc$output, function(x, nm) x[[1]]), map_chr(desc$output, "name"))

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
