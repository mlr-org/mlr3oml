CACHE = new.env(hash = FALSE, parent = emptyenv())

CACHE$versions = list(
  data = 1L,
  data_desc = 1L,
  data_qualities = 1L,
  data_features = 1L,
  task_desc = 1L,
  task_splits = 1L
)

CACHE$initialized = character()

get_cache_dir = function(cache, type = "") {
  if (isTRUE(cache)) {
    cache = R_user_dir("mlr3oml", "cache")
  }

  normalizePath(file.path(cache, type), mustWork = FALSE)
}

initialize_cache = function(cache) {
  if (isFALSE(cache)) {
    return(TRUE)
  }

  cache_dir = get_cache_dir(cache)
  if (cache_dir %in% CACHE$initialized) {
    return(TRUE)
  }

  require_namespaces("qs", "The following packages are required for caching: %s")
  cache_file = file.path(cache_dir, "version.json")
  write_cache_file = FALSE

  if (dir.exists(cache_dir)) {
    if (file.exists(cache_file)) {
      cache_versions = jsonlite::fromJSON(cache_file)
      for (type in intersect(names(cache_versions), names(CACHE$versions))) {
        if (cache_versions[[type]] != CACHE$versions[[type]]) {
          unlink(file.path(cache_dir, type), recursive = TRUE)
          write_cache_file = TRUE
        }
      }
    } else {
      stopf("Cache directory '%s' was not initialized by mlr3oml", cache_dir)
    }
  } else {
    dir.create(cache_dir, recursive = TRUE)
    write_cache_file = TRUE
  }

  if (write_cache_file) {
    writeLines(jsonlite::toJSON(CACHE$versions, auto_unbox = TRUE), con = cache_file)
  }

  CACHE$initialized = c(CACHE$initialized, cache_dir)

  return(TRUE)
}

cached = function(fun, type, id, ..., cache = FALSE) {
  if (isFALSE(cache)) {
    return(fun(id, ...))
  }

  cache_dir = get_cache_dir(cache, type)
  file = file.path(cache_dir, sprintf("%i.qs", id))

  if (file.exists(file)) {
    obj = try(qs::qread(file, nthreads = getOption("Ncpus", 1L)))
    if (!inherits(obj, "try-error"))
      return(obj)
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  obj = fun(id, ...)
  qs::qsave(obj, file = file, nthreads = getOption("Ncpus", 1L))

  return(obj)
}
