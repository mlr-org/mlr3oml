test_that("Can benchmark <-> collection", {
  with_test_server()
  tasks = lapply(c("penguins", "sonar"), tsk)
  tasks = map(c(1197L, 403L), function(x) tsk("oml", task_id = x))
  resamplings = map(c(1197L, 403L), function(x) rsmp("oml", task_id = x))
  learners = lrns(c("classif.featureless", "classif.rpart"))

  design = benchmark_grid(tasks, learners, resamplings)
  print(design)
  set.seed(123)
  bmr = benchmark(design)
  publish(bmr)

  publish(bmr$learners$learner[[1]])
})
