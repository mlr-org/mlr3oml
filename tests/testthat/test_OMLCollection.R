# skip_on_cran()

test_that("OMLCollection CC-18", {
  with_public_server()
  if (FALSE) {
    public_server()
  }
  id = 276
  c = OMLCollection$new(id)
  tasks_dt = as.data.table(c$tasks)
  expect_data_table(tasks_dt)
  flows_dt = as.data.table(c$flows)
  expect_data_table(flows_dt)
  data_dt = as.data.table(c$dat)
  expect_data_table(data_dt)
  runs_dt = as.data.table(c$runs)
  expect_data_table(runs_dt)

  expect_equal(c$name, "CC18-Example")
  expect_equal(c$creator, 869L)
  # has no creation_date
  # has no alias
  # has no tag
  expect_equal(c$main_entity_type, "run")
  expect_equal(c$description, "Description")
  expect_equal(c$flow_ids, 18995L)
  expect_equal(c$data_ids, c(1063L, 1590))
  expect_equal(c$run_ids, c(10560782L, 10560783L))
  expect_equal(c$task_ids, c(3913L, 7592L))
  expect_r6(c$runs, "OMLContainer")
  expect_r6(c$flows, "OMLContainer")
  expect_r6(c$data, "OMLContainer")
  expect_r6(c$tasks, "OMLContainer")
  expect_true(c$runs$contains == "OMLRun")
  expect_true(c$flows$contains == "OMLFlow")
  expect_true(c$data$contains == "OMLData")
  expect_true(c$tasks$contains == "OMLTask")
})
id = 15
c = OMLCollection$new(id)
c$flows$get(1)

test_that("Can benchmark <-> collection", {
  with_test_server()
  tasks = lapply(c("penguins", "sonar"), tsk)
  tasks = list(OMLTask$new(1197L)$convert(), OMLTask$new(403L)$convert())
  learners = lapply(c("classif.featureless", "classif.rpart"), lrn)
  resamplings = rsmp("cv", folds = 3)

  design = benchmark_grid(tasks, learners, resamplings)
  print(design)

  set.seed(123)
  bmr = benchmark(design)
  debugonce(publish.BenchmarkResult)
  publish(bmr)

  debugonce(publish)
  publish(bmr$learners$learner[[1]])
})
