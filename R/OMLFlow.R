#' @title Interface to OpenML Flows
#'
#' @description
#' This is the class for flows served on [OpenML](https://new.openml.org/search?type=flow).
#' Flows are similar to pipelines as defined in the mlr3pipeline package.
#' Uploading of learners to OpenML has currently only limited supported but can be extended on
#' request.
#'
#' @section mlr3 Integration:
#' * Obtain a [mlr3::Learner] using `as_learner()`. mlr3 Flows are converted into proper mlr3
#' learners, while other flows are converted to non-executable pseudo-learners, where the
#' task type has to be passed to `as_task()` as OpenML Flows are task-agnostic. While these learners
#' are not directly useful, this allows sklearn runs to be converted to [mlr3::ResampleResult]s and
#' run collections can be converted to [mlr3::BenchmarkResult]s. Their paramset corresponds to the
#' union of the parameter set of the flow and it's subcomponents, where the id of the component
#' is appended to the name of the parameter.
#'
#' @references
#' `r format_bib("vanschoren2014")`
#'
#' @export
#' @examples
#' \donttest{
#' # mlr3 flow:
#' flow = OMLFlow$new(id = 19082L)
#' learner = as_learner(flow)
#' # python flow
#' python_flow = OMLFlow$new(19090L)
#' # conversion to pseudo Learner
#' plearner = as_learner(python_flow, "classif")
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
    #' @param id (`integeger(1)`) The id of the Flow
    #' @param cache (`logical(1)`) whether to use caching.
    initialize = function(id, cache = getOption("mlr3oml.cache", FALSE)) {
      self$id = assert_count(id, coerce = TRUE)
      self$cache_dir = get_cache_dir(assert_flag(cache))
      initialize_cache(self$cache_dir)
    },

    #' @description
    #' Prints the object.
    print = function() {
      catf("<OMLFlow:%i>", self$id)
      catf(" * Name: %s", truncate_name(self$name))
      catf(" * Dependencies: %s", paste(self$desc$dependencies, collapse = ", "))
    }
  ),
  active = list(
    #' @field desc (`list()`)\cr
    #' The description as downloaded from OpenML.
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = cached(download_flow_desc, type = "flow", id = self$id,
          cache_dir = self$cache_dir
        )
      }
      private$.desc
    },

    #' @field parameters (`data.table`)\cr
    #' The parameters of the flow.
    parameters = function() self$desc$parameter,

    #' @field tags (`character()`)\cr
    #' The tags of the flow.
    tags = function() self$desc$tag,

    #' @field dependencies (`character()`)\cr
    #' The dependencies of the flow.
    dependencies = function() self$desc$dependencies,

    #' @field name (`character(1)`)\cr
    #' The name of the flow.
    name = function() self$desc$name
  ),
  private = list(
    .desc = NULL
  )
)

#' @title Convert an OpenML Flow to a mlr3 Learner
#' @param task_type (`character(1)`)
#'    The task type to constrct a pseudo-learner. For more information see [mlr3oml::OMLFlow].
#' @param from_binary (`logical(1)`) Whether to construct the learner from the binary rds file
#'    that is provided on OpenML. Note that when learners are constructed like this and the
#'    versions of the installed packages don't match the flow's dependencies, the resulting
#'    flow (and therefore also the resulting [mlr3::ResampleResult]s) cannot be published on OpenML.
#'    If it is FALSE, the function tries to construct the learner using `lrn()`.
#' @importFrom mlr3 as_learner
#' @export
as_learner.OMLFlow = function(x, task_type = NULL, from_binary = FALSE, verbose = TRUE, ...) {
  assert_choice(task_type, c("regr", "classif", "surv"), null.ok = TRUE)
  assert_flag(from_binary)
  is_mlr3 = startsWith(x$name, "mlr3")

  if (is_mlr3 && !from_binary) {
    mlr3_id = substr(x$name, 6L, nchar(x$name))
    require_namespaces(get_mlr3_package(x))
    learner = try(lrn(mlr3_id), silent = TRUE)
    if (inherits(learner, "try-error")) {
      messagef(
        paste(
          "Could not create learner using lrn(), try attaching the corresponding mlr3 package.",
          "Returning NULL.",
          sep = "\n"
        )
      )
      return(NULL)
    }
  }

  if (is_mlr3 && from_binary) {
    learner = cached(download_flow_binary, "learner", x$id, cache_dir = x$cache_dir,
      desc = x$desc
    )
    # in case the versions don't match and the flow was downloaded from OpenML, the
    if (check_dependencies(x, verbose)) { # this throws appropriate warnings and returns TRUE if everyhing
      # is correctly installed.
      learner$.__enclos_env__$private$oml_id = flow$id
    } else {
      # This is checked before publishing the flow
      learner$.__enclos_env__$private$oml_id = "dependency_mismatch"
    }
  }

  if (!is_mlr3) {
    learner = make_oml_learner(x, task_type)
    if (verbose) {
      messagef("Constructed non-executable pseudo-learner.")
    }
  }
  return(learner)
}

#' When constructing the learner using lrn(), this function already indicates missing packages.
#' This function throws a warning in case the versions of the installed packages don't match the
#' dependencies of the flow on OpenML
check_dependencies = function(flow, verbose) {
  # Idea: Get the versoins of the installed packages, compare it to the versions of the dependencies
  # and throw a wwarning if they don' match
  dependencies = flow$dependencies
  is_R = startsWith(dependencies, "R_")
  R_version = dependencies[is_R]
  R_version_installed = paste0("R", paste0(R.Version()[c("major", "minor")], collapse = "."))
  if (length(R_version) && (R_version != R_version_installed && verbose)) {
    warningf("Flow's R version is %s.", R_version)
  }

  dependencies = dependencies[!is_R]
  pkgs_flow = map_chr(strsplit(dependencies, split = "_"), 1)
  versions_flow = map_chr(strsplit(dependencies, split = "_"), 2)
  deps_flow = set_names(versions_flow, pkgs_flow)

  installed_pkgs = installed.packages()
  installed_pkgs = installed_pkgs[rownames(installed_pkgs) %in% pkgs_flow, ]
  installed_pkgs = paste(installed_pkgs[, "Package"], installed_pkgs[, "Version"], sep = "_")

  if (!length(installed_pkgs)) {
    if (verbose) {
      warningf("Missing all required dependencies: %s", paste(dependencies, collapse = ", "))
    }
    return(FALSE)
  }

  pkgs_lrn = map_chr(strsplit(installed_pkgs, split = "_"), 1)
  versions_lrn = map_chr(strsplit(installed_pkgs, split = "_"), 2)
  deps_lrn = set_names(versions_lrn, pkgs_lrn)

  everything_correct = length(dependencies) == length(installed_pkgs) &&
    all(sort(dependencies) == sort(installed_pkgs)) && R_version == R_version_intalled

  if (everything_correct) {
    return(TRUE)
  }

  # now two cases: either all are correctly installed or not
  missing_pkgs = setdiff(pkgs_flow, pkgs_lrn)

  if (length(missing_pkgs) && verbose) {
    warningf(
      "Flow has has uninstalled dependencies: %s",
      paste(paste(missing_pkgs, deps_flow[missing_pkgs], sep = "_"), collapse = ", ")
    )
  }

  pkgs_with_conflicts = names(which(deps_lrn[pkgs_lrn] != deps_flow[pkgs_lrn]))
  warning = map(
    pkgs_with_conflicts,
    function(pkg) {
      paste0(" * ", pkg, ": ", deps_lrn[pkg], " != ", deps_flow[pkg])
    }
  )
  warning = paste0("Version conflicts (installed != required)\n", paste(warning, collapse = "\n"))
  warning = paste(
    warning,
    "Due to these conflicts the constructed learner cannot be published",
    sep = "\n"
  )
  if (verbose) {
    warningf(warning)

  }
  return(FALSE)
}

check_missing_packages = function(flow) {
  pkgs_flow = map_chr(strsplit(flow$dependencies, split = "_"), 1)
  versions_flow = map_chr(strsplit(flow$dependencies, split = "_"), 2)
  deps_flow = set_names(paste(pkgs_flow, versions_flow, sep = "_"), pkgs_flow)

  installed_pkgs = installed.packages()
  installed_pkgs = installed_pkgs[rownames(installed_pkgs) %in% pkgs_flow, , drop = FALSE]
  installed_pkgs = unname(installed_pkgs[, "Package"])

  if (length(installed_pkgs) != length(pkgs_flow)) {
    missing_pkgs = setdiff(pkgs_flow, installed_pkgs)
    warningf(
      paste0(
        "Missing packages for flow: ",
        paste(deps_flow[missing_pkgs], collapse = ", ")
      )
    )
  }
  return(NULL)
}
