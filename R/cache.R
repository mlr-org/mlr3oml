#' CACHE
#' This keeps track of the cache versions.
#' When incremented in a release, it ensures that the previous cache gets flushed, thereby
#' allowing to easily change the caching mechanism / structure of files in the future.
#' @noRd
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

#' Returns the cache directory
#' @param cache Whether to cache.
#' @param test_server Whether to use the test server.
#' @noRd
get_cache_dir = function(cache, test_server) {
  assert_true(is.logical(cache) || is.character(cache))
  if (isFALSE(cache)) {
    return(FALSE)
  }
  if (!is.character(cache)) {
    cache = R_user_dir("mlr3oml", "cache")
  }

  assert(check_directory_exists(cache), check_path_for_output(cache))
  normalizePath(cache, mustWork = FALSE)
}

#' Initializes the cache directory.
#' When a cached is initialized in a session, it is added to the `CACHE` environment, and we trust
#' it without checking the cache versions.
#' Otherwise we compare the written cache versions for the subfolders like `data_desc` with the
#' current CACHE versions of the mlr3oml package. If they differ, we flush the cache and initialized
#' a new folder with the updated cache version.
#'
#' @noRd
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

          # the first unlink we need to get rid of the old cached files from version <= 0.5.0
          unlink(file.path(cache_dir, type), recursive = TRUE)
          unlink(file.path(cache_dir, "test", type), recursive = TRUE)
          unlink(file.path(cache_dir, "public", type), recursive = TRUE)
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

#' @title Cached
#'
#' @description
#' This function performs a cached version of the function 'fun'. I.e. it first checks whether
#' the objects is already stored in cache and returns it, otherwise it downloads it and stores it
#' in the cache folder. It keeps different caches for the public server and the test server.
#' The arguments test_server and server are usually both passed,
#'
#' @param fun (`function`)\cr
#'   Download function, e.g. download_desc_data
#' @param type (`character(1)`)\cr
#'   The type of object that is downloaded by fun. This is used to determine the caching folder.
#' @param id (`integer(1)`)\cr
#'   The id of the object that is being downloaded.
#' @param test_server (`logical(1)`)\cr
#'   Whether to use the test server. This is needed to determine the cache directory.
#' @param parquet (`logical(1)`)\cr
#'   Whether the caching is done for parquet, in this case we don't use qs to compress the data
#'   and the caching therefore works a little different.
#' @param cache_dir (`character(1)`)\cr
#'   The cache directory.
#' @param ... (any)\cr
#'   Additional arguments passed to `fun(id, ...)`.
#'
#' @noRd
cached = function(fun, type, id, test_server, parquet = FALSE, ..., cache_dir = FALSE) {
  if (isFALSE(cache_dir)) {
    return(fun(id, ...))
  }

  path = file.path(cache_dir, ifelse(test_server, "test", "public"), type)
  file = file.path(path, sprintf("%i.%s", id, if (parquet) "parquet" else "qs"))

  if (file.exists(file)) {
    if (parquet) {
      lg$debug("Returning parquet path.", type = type, id = id, file = file)
      return(file)
    } else {
      lg$debug("Loading object from cache", type = type, id = id, file = file)
      obj = try(qs::qread(file, nthreads = getOption("Ncpus", 1L)))
      if (!inherits(obj, "try-error")) {
        return(obj)
      }
      lg$debug("Failed to retrieve object from cache", type = type, id = id, file = file)
    }
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  obj = fun(id, ...)
  if (parquet) {
    lg$debug("Moving parquet data from tempfile to cache.", type = type, id = id, file = file)
    file.rename(obj, file)
    return(file)
  }

  lg$debug("Storing compressed object in cache", type = type, id = id, file = file)
  qs::qsave(obj, file = file, nthreads = getOption("Ncpus", 1L))
  return(obj)
}
