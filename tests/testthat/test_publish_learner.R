skip_on_cran()

test_that("Uploading flow works", {
  with_test_server()
  learner = mlr3::lrn("regr.rpart")
  response = publish(learner)
  print(response)
})
