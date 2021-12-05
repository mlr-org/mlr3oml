#' @title Interface to OpenML Flows
#'
#' @description
#' This is the class for tasks provided on \url{https://openml.org/}.
#' For a description of the full xsd scheme see here:
#' https://www.openml.org/api/v1/xsd/openml.implementation.upload.
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
    #' Tries to convert the flow to a mlr3 object.
    convert = function() {
      flow2mlr3(self)
    },

    print = function() {
      catf("<OMLFlow:%i>", self$id)
    }
  ),
  active = list(
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = download_flow_desc(self$id)
      }
      private$.desc
    },
    # TODO: document fields
    upload_date = function() self$desc$upload_date,
    description = function() self$desc$description,
    language = function() self$desc$language,
    parameter = function() self$desc$parameter,
    tag = function() self$desc$tag,
    version = function() self$desc$version,
    version_label = function() self$desc$version_label,
    dependencies = function() self$desc$dependencies,
    uploader = function() self$desc$uploader,
    uploader_name = function() self$desc$uploader_name,
    name = function() self$desc$name,
    full_description = function() self$desc$full_description
  ),

  private = list(
    .desc = NULL
  )
)


flow2mlr3 = function(flow) {
  # use grepl in case versions are addeda
  mlr3_deps = flow$dependencies[grep("mlr3", flow$dependencies)]
  other_deps = setdiff(flow$dependencies, mlr3_deps)
  assert_true(length(mlr3_deps) > 0)

  # First check for the mlr3deps to be able to initialize the learner
  if (!requireNamespace(mlr3_deps)) {
    mlr3misc::stopf("Install the required mlr3 packages: %s.",
         paste(mlr3_deps, collapse = ", "))
  }

  if (length(other_deps) && !requireNamespace(other_deps)) {
    warning("Install the required packages with mlr3extralearners::install_learners().")
  }

  if ("mlr3pipelines" %in% mlr3_deps) {
    return(mlr3pipelines::Graph$new())
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

