test_that("Can create valid pseudo learner", {
  id = 1183L
  flow = OMLFlow$new(id)
  params = construct_paramset(flow$parameter)
  expect_r6(params, "ParamSet")
  expect_true(nrow(as.data.table(params)) == 20L)
  task_type = "classif"
  learner = make_oml_learner(flow, task_type)
  expect_r6(learner, sprintf("Learner%sOML", capitalize(task_type)))
  expect_true(learner$id == sprintf("%s.oml_1183", task_type))
  task_type = "classif"
  learner = make_oml_learner(flow, task_type)
  expect_r6(learner, sprintf("Learner%sOML", capitalize(task_type)))
  expect_true(learner$id == sprintf("%s.oml_1183", task_type))
})
