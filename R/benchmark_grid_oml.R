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
#' @examples
#' \donttest{
#' library("mlr3")
#' collection = OMLCollection$new(258)
#' otasks = collection$tasks[1:2, ][["task"]]
#' tasks = as_tasks(otasks)
#' resamplings = as_resamplings(otasks)
#' learners = lrns(c("classif.rpart", "classif.featureless"))
#' design = benchmark_grid_oml(tasks, learners, resamplings)
#' print(design)
#' bmr = benchmark(design)
#' }
#' @return ([`data.table()`])
#' @export
benchmark_grid_oml = function(tasks, learners, resamplings) {
  tasks = assert_tasks(as_tasks(tasks))
  learners = assert_learners(as_learners(learners))
  resamplings = assert_resamplings(as_resamplings(resamplings))
  assert_true(length(tasks) == length(resamplings))
  assert_true(all(map_lgl(resamplings, "is_instantiated")))
  same_hash = function(task, resampling) task$hash == resampling$task_hash
  assert(all(pmap_lgl(list(tasks, resamplings), same_hash)))

  grid = CJ(task = seq_along(tasks), learner = seq_along(learners))
  grid$instance = seq_row(grid)

  design = data.table(
    task = tasks[grid$task],
    learner = learners[grid$learner],
    resampling = resamplings[grid$task]
  )

  return(design)
}
