test_that("Learner", {
  lrn = lrn("regr.rpart")
  debugonce(make_description)
  desc = make_description(lrn)
})

test_that("Graph Learner", {
  imputehist = mlr3pipelines::po("imputehist")
  pca = mlr3pipelines::po("pca")
  learner_po = mlr3pipelines::po("learner", learner = lrn("classif.rpart"))
  graph = imputehist %>>% pca %>>% learner_po
  glrn = GraphLearner$new(graph)

  debugonce(make_description.GraphLearner)
  desc = make_description(glrn)
})

test_that("AutoTuner", {

})
