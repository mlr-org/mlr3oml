#' Helper function to create a benchmark design
#'
#' @description [mlr3oml::OMLTasks]s contain tasks as well as resamplings. In order to create
#' a benchmark design from a list of tasks and corresponding instantiated resamplings, this
#' function can be used.
#'
#' @param tasks list of [mlr3::Task]s
#' @param learners list of [mlr3::Learner]s
#' @param resamplings list of [mlr::Resampling]s for the tasks.
#'
#' @export
#'
benchmark_design = function(tasks, learners, resamplings) {
  tasks = assert_tasks(mlr3::as_tasks(tasks))
  learners = assert_learners(mlr3::as_learners(learners))
  resamplings = assert_resamplings(mlr3::as_resamplings(resamplings))
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
