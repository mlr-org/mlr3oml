CACHE = new.env(hash = FALSE, parent = emptyenv())

CACHE$versions = list(
  data = 2L,
  data_parquet = 2L,
  data_desc = 2L,
  data_qualities = 2L,
  data_features = 2L,
  task_desc = 2L,
  task_splits = 2L,
  flow_desc = 2L,
  collection_desc = 2L,
  run_desc = 2L,
  prediction = 2L
)

CACHE$initialized = character()

get_cache_dir = function(cache, test_server) {
  assert_true(is.logical(cache) || is.character(cache))
  if (isFALSE(cache)) {
    return(FALSE)
  }
  if (!is.character(cache)) {
    cache_root = R_user_dir("mlr3oml", "cache")
  } else {
    cache_root = cache
  }

  cache_dir = file.path(cache_root, ifelse(test_server, "test", "public"))

  assert(
    check_directory_exists(cache_dir),
    check_path_for_output(cache_dir),
    check_path_for_output(cache_root)
    # because we create the sub-directories public / test  ourselves,
    # it is enough if the cache root, relative to which ./public and ./test are
    # is a path for output
  )

  normalizePath(cache_dir, mustWork = FALSE)
}

initialize_cache = function(cache_dir, test_server) {
  if (isFALSE(cache_dir) || cache_dir %in% CACHE$initialized) {
    lg$debug("Skipping initialization of cache", cache_dir = cache_dir)
    return(TRUE)
  }

  require_namespaces("qs", "The following packages are required for caching: %s")

  write_cache_file = FALSE
  version_file = file.path(cache_dir, "version.json")
  server_file = file.path(cache_dir, "server.json")
  server = ifelse(test_server, "test", "public")

  if (dir.exists(cache_dir)) {
    if (file.exists(server_file)) {
      cache_server = jsonlite::fromJSON(server_file)
      if (cache_server != server) {
        stopf("You are mixing cache directories of public and test server.")
      }
    }

    if (!file.exists(version_file)) {
      stopf("Cache directory '%s' was not initialized by mlr3oml", cache_dir)
    } else {
      cache_versions = jsonlite::fromJSON(version_file)
      for (type in intersect(names(cache_versions), names(CACHE$versions))) {
        if (cache_versions[[type]] != CACHE$versions[[type]]) {
          lg$debug("Invalidating cache dir due to a version mismatch", path = file.path(cache_dir, type))
          unlink(file.path(cache_dir, type), recursive = TRUE)
          write_cache_file = TRUE
        }
      }
    }

  } else {
    dir.create(cache_dir, recursive = TRUE)
    write_cache_file = TRUE
  }

  if (write_cache_file) {
    lg$debug("Writing cache version information", path = version_file)
    writeLines(jsonlite::toJSON(CACHE$versions, auto_unbox = TRUE), con = version_file)
    lg$debug("Writing cache server information", path = server_file)
    writeLines(jsonlite::toJSON(server, auto_unbox = TRUE), con = server_file)
  }

  CACHE$initialized = c(CACHE$initialized, cache_dir)

  return(TRUE)
}

# @title Cached
#
# @description
# This function performs a cached version of the function 'fun'. I.e. it first checks whether
# the objects is already stored in cache and returns it, otherwise it downloads it and stores it
# in the cache folder. It keeps different caches for the public server and the test server.
#
# @param fun
#   Download function, e.g. download_desc_data
# @param type
#   The type of object that is downloaded by fun. This is the subfolder where the object will be
#   stored (relative to the cache folder R_user_dir("mlr3oml", "cache")).
# @param id
#   The id of the object that is being downloaded, this can also be an url, as this is needed
#
#
# @param type
# The type that is downloaded, not really necessary
cached = function(fun, type, id, ..., cache_dir = FALSE) {
  if (isFALSE(cache_dir)) {
    return(fun(id, ...))
  }

  path = file.path(cache_dir, type)
  file = file.path(path, sprintf("%i.qs", id))

  if (file.exists(file)) {
    lg$debug("Loading object from cache", type = type, id = id, file = file)
    obj = try(qs::qread(file, nthreads = getOption("Ncpus", 1L)))
    if (!inherits(obj, "try-error")) {
      return(obj)
    }
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
