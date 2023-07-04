#' @title Abstract Base Class for OpenML objects.
#'
#' @name oml_object
#'
#' @description
#' All OML Objects inherit from this class.
#' Don't use his class directly.
#'
#' @export
OMLObject = R6Class("OMLObject",
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_id
    #' @template param_test_server
    #' @param type (`charcater()`)\cr
    #'   The type of OpenML object (e.g. run, task, ...).
    initialize = function(
      id,
      test_server = test_server_default(),
      type
      ) {
      private$.type = assert_choice(type, c("data", "flow", "study", "collection", "run", "task"))
      private$.test_server = assert_flag(test_server)
      private$.server = get_server(test_server)

      private$.id = assert_count(id, coerce = TRUE)
      private$.cache_dir = get_cache_dir(getOption("mlr3oml.cache", FALSE))
      initialize_cache(private$.cache_dir)
    },
    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    }
  ),
  active = list(
    #' @field desc (`list()`)\cr
    #' Description of OpenML object.
    desc = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(private$.desc)) {
        private$.desc = cached(
          get_desc_downloader(self$type),
          server = self$server, type = sprintf("%s_desc", private$.type), self$id,
          cache_dir = self$cache_dir, test_server = self$test_server
        )
      }

      private$.desc
    },
    #' @template field_cache_dir
    cache_dir = function(rhs) {
      assert_ro_binding(rhs)
      if (!isFALSE(private$.cache_dir) && !dir.exists(private$.cache_dir)) {
        # e.g. an OMLObject on different PCs
        prev_cache_dir = private$.cache_dir
        private$.cache_dir = get_cache_dir(getOption("mlr3oml.cache", FALSE))
        lg$info(sprintf("Cache directory '%s' changed since initializing this object and is now '%s'.",
          prev_cache_dir, private$.cache_dir)) # nolint
        initialize_cache(private$.cache_dir)
      }

      private$.cache_dir
    },
    #' @field id (`integer(1)`)\cr
    #' OpenML data id.
    id = function(rhs) {
      assert_ro_binding(rhs)
      private$.id
    },
    #' @field server (`character(1)`)\cr
    #' The server for this object.
    server = function(rhs) {
      assert_ro_binding(rhs)
      private$.server
    },
    #' @field man (`character(1)`)\cr
    #' The manual entry.
    man = function(rhs) {
      assert_ro_binding(rhs)
      sprintf("mlr3oml::OML%s", capitalize(self$type))
    },
    #' @field name (`character(1)`)\cr
    #' The name of the object.
    name = function() self$desc$name,
    #' @field type (`character()`)\cr
    #' The type of OpenML object (e.g. task, run, ...).
    type = function(rhs) {
      assert_ro_binding(rhs)
      private$.type
    },
    #' @field test_server (`logical(1)`)\cr
    #' Whether the object is using the test server.
    test_server = function(rhs) {
      assert_ro_binding(rhs)
      private$.test_server
    }
  ),
  private = list(
    .desc = NULL,
    .cache_dir = NULL,
    .id = NULL,
    .server = NULL,
    .type = NULL,
    .test_server = NULL
  )
)
