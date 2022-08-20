publish.BenchmarkResult = function(x, api_key = get_api_key(), ...) {
  rrs = bmr$resample_results$resample_result
  ids = list()
  for (rr in rrs)  {
    id = publish(rr)
    ids = append(ids, id)
  }



}
