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

get_cache_dir = function(cache) {
  if (isFALSE(cache))
    return(FALSE)

  if (isTRUE(cache))
    cache = R_user_dir("mlr3oml", "cache")

  assert(check_directory_exists(cache), check_path_for_output(cache))
  normalizePath(cache, mustWork = FALSE)
}

initialize_cache = function(cache_dir) {
  if (isFALSE(cache_dir) || cache_dir %in% CACHE$initialized) {
    lg$debug("Skipping initialization of cache", cache_dir = cache_dir)
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
          lg$debug("Invalidating cache dir due to a version mismatch", path = file.path(cache_dir, type))
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
    lg$debug("Writing cache version information", path = cache_file)
    writeLines(jsonlite::toJSON(CACHE$versions, auto_unbox = TRUE), con = cache_file)
  }

  CACHE$initialized = c(CACHE$initialized, cache_dir)

  return(TRUE)
}

cached = function(fun, type, id, ..., cache_dir = FALSE) {
  if (isFALSE(cache_dir)) {
    return(fun(id, ...))
  }

  path = file.path(cache_dir, type)
  file = file.path(path, sprintf("%i.qs", id))

  if (file.exists(file)) {
    lg$debug("Loading object from cache", type = type, id = id, file = file)
    obj = try(qs::qread(file, nthreads = getOption("Ncpus", 1L)))
    if (!inherits(obj, "try-error"))
      return(obj)
    lg$debug("Failed to retrieve object from cache", type = type, id = id, file = file)
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  obj = fun(id, ...)
  lg$debug("Storing object in cache", type = type, id = id, file = file)
  qs::qsave(obj, file = file, nthreads = getOption("Ncpus", 1L))

  return(obj)
}
