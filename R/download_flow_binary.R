download_flow_binary = function(flow_id, desc = download_flow_desc(flow_id)) {
  get_rds(desc$binary_url)
}
