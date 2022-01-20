#' @title Interface to OpenML Flows
#'
#' @description
#' This is the class for flows provided on the [OpenML website](https://new.openml.org/search?type=flow&sort=runs).
#'
#' @references
#' `r format_bib("vanschoren2014")`
#'
#' @export
#' @examples
#' \donttest{
#' flow = OMLFlow$new(id = 1)
#' }
OMLFlow = R6Class("OMLFlow",
  public = list(
    #' @field id (`integer(1)`)\cr
    #' OpenML flow id.
    id = NULL,

    #' @template field_cache_dir
    cache_dir = NULL,

    #' @description
    #' Initializes a new object of class [mlr3oml::OMLFlow].
    initialize = function(id, cache = getOption("mlr3oml.cache", FALSE)) {
      self$id = assert_count(id, coerce = TRUE)
      self$cache_dir = get_cache_dir(assert_flag(cache))
      initialize_cache(self$cache_dir)
    },

    #' @description
    #' Tries to convert the OMLFlow into an [mlr3::Learner] if the name starts with mlr3.
    convert = function() {
      file = tempfile(fileext = ".rds")
      withr::defer(unlink(file))
      learner = tryCatch(get_rds(self$desc$binary_url),
        error = function(cond) {
          warning("Could not convert flow, returning NULL.")
          return(NULL)
        }
      )
      learner$.__enclos_env__$private$oml_id = self$id
      return(learner)
    },

    #' @description
    #' Prints the object.
    print = function() {
      catf("<OMLFlow:%i>", self$id)
    }
  ),
  active = list(
    #' @field desc (`list(n)`)\cr
    #' The description as downloaded from OpenML.
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = download_flow_desc(self$id)
      }
      private$.desc
    },

    #' @field parameter (`data.table`)\cr
    #' The parameters of the flow.
    parameter = function() self$desc$parameter,

    #' @field tag (`character(n)`)\cr
    #' The tags of the flow.
    tag = function() self$desc$tag,

    #' @field dependencies (`character(n)`)\cr
    #' The dependencies of the flow.
    dependencies = function() self$desc$dependencies,

    #' @field name (`character(1)`)\cr
    #' The name of the flow.
    name = function() self$desc$name,

    #' @field description (`character(1)`)\cr
    #' The description of the flow.
    description = function() self$desc$description
  ),
  private = list(
    .desc = NULL
  )
)

#' @importFrom mlr3 as_learner
#' @export
as_learner.OMLFlow = function(flow) {
  # use grepl in case versions are addeda

  dependencies = sub("_[^_]+$", "", flow$dependencies)
  mlr3_deps = dependencies[grep("mlr3", dependencies)]
  other_deps = setdiff(dependencies, mlr3_deps)
  assert_true(length(mlr3_deps) > 0)

  # First check for the mlr3deps to be able to initialize the learner
  if (!requireNamespace(mlr3_deps)) {
    mlr3misc::stopf(
      "Install the required mlr3 packages: %s.",
      paste(mlr3_deps, collapse = ", ")
    )
  }

  if (length(other_deps) && !requireNamespace(other_deps)) {
    warning("Install the required packages with mlr3extralearners::install_learners().")
  }

  lrn_id = get_lrn_id(flow$name)
  # loop over all packages (if e.g. deps are mlr3 and mlr3pipelines) and see where the
  # class lives
  for (pkg in mlr3_deps) {
    lrn = construct_lrn(pkg = pkg, lrn_id = lrn_id)
    if (class(lrn)[[1]] != "try-error") break
  }
  return(lrn)
}


get_lrn_id = function(name) {
  lrn_id = strsplit(name, split = "\\.")[[1]][-1]
  lrn_id = Reduce(function(x, y) paste(x, y, sep = "."), lrn_id)
  return(lrn_id)
}

construct_lrn = function(pkg, lrn_id) {
  if (!requireNamespace(pkg)) {
    mlr3misc::stopf("Package %s required not not installed.", pkg)
  }
  learner = lrn(lrn_id)
  return(learner)
}
