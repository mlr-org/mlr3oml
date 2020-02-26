library(fst)
library(qs)

roundtrip_base = function(data) {
  path = tempfile()
  saveRDS(data, path)
  readRDS(path)
}

roundtrip_fst = function(data) {
  path = tempfile()
  write.fst(data, path)
  read.fst(path)
}

roundtrip_qs = function(data) {
  path = tempfile()
  qsave(data, path, preset = "fast", nthreads = 4, check_hash = FALSE)
  qread(path)
}

library(nycflights13)
data = as.data.frame(flights)

bench::mark(
  roundtrip_base(data),
  roundtrip_fst(data),
  roundtrip_qs(data)
)
