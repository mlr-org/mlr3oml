#' @import checkmate
#' @import data.table
#' @import mlr3misc
#' @import mlr3
#' @importFrom R6 R6Class
#' @importFrom methods hasArg
#' @importFrom stats rnorm
#' @importFrom utils installed.packages tail
#' @importFrom bit64 integer64
#'
#'
#' @section Documentation:
#' Start by reading the Large-Scale Benchmarking
#' [chapter](https://mlr3book.mlr-org.com/chapters/chapter11/large-scale_benchmarking.html)
#' from the mlr3book.
#'
#' @section mlr3 Integration:
#' This package adds the [mlr3::Task] `"oml"` and the [mlr3::Resampling] `"oml"` to
#' [mlr3::mlr_tasks] and [mlr3::mlr_resamplings], respectively.
#' For the former you may pass either a `data_id` or a `task_id`, the latter requires
#' a `task_id`.
#' Furthermore it allows to convert the OpenML objects to mlr3 objects using the usual S3 generics
#' such as [mlr3::as_task], [mlr3::as_learner], [mlr3::as_resampling], [mlr3::as_resample_result],
#' [mlr3::as_benchmark_result] or [mlr3::as_data_backend]. This allows for a frictionless
#' integration of OpenML and mlr3.
#'
#' @section Options:
#' * `mlr3oml.cache`: Enables or disables caching globally.
#'   If set to `FALSE`, caching is disabled.
#'   If set to `TRUE`, cache directory as reported by [R_user_dir()] is used.
#'   Alternatively, you can specify a path on the local file system here.
#'   Default is `FALSE`.
#' * `mlr3oml.api_key`: API key to use. All operations supported by this package
#'   work without an API key, but you might get rate limited without an API key.
#'   If not set, defaults to the value of the environment variable `OPENMLAPIKEY`.
#' * `mlr3oml.arff_parser`: ARFF parser to use, defaults to the internal one relies
#'   on [data.table::fread()]. Can also be set to `"RWeka"` for the parser in
#'   \CRANpkg{RWeka}.
#' * `mlr3oml.parquet`: Enables or disables parquet as the default file format.
#'   If set to `TRUE`, the parquet version of datasets will be used by default.
#'   If set to `FALSE`, the arff version of datasets will be used by default.
#'   Note that the OpenML sever is still transitioning from arff to parquet and some features
#'   will work better with arff.
#'   Default is `FALSE`.
#'  * `mlr3oml.retries`:
#'  An integer defining number of retries when downloading data from OpenML.
#'  If it is `NULL`, the number of retries is set to 3.
#'
#' **Relevant for developers**
#'
#' * `mlr3oml.test_server`:
#'   The default value for whether to use the OpenML test server.
#'   Default is `FALSE`.
#' * `mlr3oml.test_api_key`:
#'   API key to use for the test server. If not set, defaults to the value of the environment
#'   variable `TESTOPENMLAPIKEY`.
#'
#' @section Logging:
#' The \CRANpkg{lgr} package is used for logging.
#' To change the threshold, use `lgr::get_logger("mlr3oml")$set_threshold()`.
"_PACKAGE"

# To silence RCMD CHECK
utils::globalVariables(c("super"))

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)
  backports::import(pkgname, "R_user_dir", force = TRUE)
  mlr3::mlr_tasks$add("oml", OMLTaskConnector)
  mlr3::mlr_resamplings$add("oml", OMLResamplingConnector)

  # setup logger
  assign("lg", lgr::get_logger(pkgname), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }

  utils::globalVariables(c("is_target", "is_ignore", "is_row_identifier"), pkgname)
} # nocov end

.onUnload = function(libpath) { # nolint
  # nocov start
  library.dynam.unload("mlr3oml", libpath)
  mlr_tasks$remove("oml")
  mlr_resamplings$remove("oml")
} # nocov end

leanify_package()
