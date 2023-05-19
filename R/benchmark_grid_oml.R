#' @title Helper function to create a benchmark design
#'
#' @description [mlr3oml::OMLTask]s contain tasks as well as resamplings. In order to create
#' a benchmark design from a list of tasks and corresponding instantiated resamplings, this
#' function can be used.
#'
#' @param tasks (`list()` or `Task`) A list of [mlr3::Task]s.
#' @param learners (`list()` or `Learner`) A list of [mlr3::Learner]s.
#' @param resamplings (`list()` or `Resampling`) A list of [mlr3::Resampling]s that are instantiated on the given tasks.
#'
#' @return ([`data.table()`])
#' @export
benchmark_grid_oml = function(tasks, learners, resamplings) {
  .Deprecated("mlr3::benchmark_grid(..., paired = TRUE)")
  mlr3::benchmark_grid(tasks, learners, resamplings, paired = TRUE)
}
