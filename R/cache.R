get_cache_dir = function(type = "") {
  file.path(R_user_dir("mlr3oml", "cache"), type)
}

cached = function(fun, type, id, ..., use_cache = FALSE) {
  if (!use_cache) {
    return(fun(id, ...))
  }
  require_namespaces("qs", "The following packages are required for caching: %s")

  path = get_cache_dir(type)
  file = file.path(path, sprintf("%i.qs", id))

  if (file.exists(file)) {
    obj = try(qs::qread(file, nthreads = getOption("Ncpus", 1L)))
    if (!inherits(obj, "try-error"))
      return(obj)
  }

  obj = fun(id, ...)

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  qs::qsave(obj, file = file, nthreads = getOption("Ncpus", 1L))

  return(obj)
}

clear_cache = function() {
  path = get_cache_dir()
  if (dir.exists(path))
    unlink(path, recursive = TRUE)
  invisible(TRUE)
}
