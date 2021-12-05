expect_flow = function(flow) {
  checkmate::expect_r6(flow, "OMLFlow")
  checkmate::expect_integer(flow$id)
  checkmate::expect_posixct(flow$upload_date)
  checkmate::expect_string(flow$description)
  checkmate::expect_data_table(flow$parameter)
  checkmate::expect_equal(names(flow$parameter),
                          c("name", "data_type", "default_value", "description")
  checkmate::expect_character(flow$dependencies)
  checkmate::expect_integer(flow$uploader)
  checkmate::expect_integer(flow$id)
  checkmate::expect_string(flow$name)
  checkmate::expect_integer(flow$version)
}
