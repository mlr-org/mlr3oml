cached = function(fun, type, id, ..., use_cache = FALSE) {
  if (!use_cache) {
    return(fun(id, ...))
  }
  require_namespaces("qs", "The following packages are required for caching: %s")

  path = file.path(R_user_dir("mlr3oml", "cache"), type)
  file = file.path(path, sprintf("%i.qs", id))

  if (file.exists(file)) {
    return(qs::qread(file, nthreads = getOption("Ncpus", 1L)))
  }

  obj = fun(id, ...)

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  qs::qsave(obj, file = file, nthreads = getOption("Ncpus", 1L))

  return(obj)
}
