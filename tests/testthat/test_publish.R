testthat("Can publish, download and convert", {
  with_test_server()
  withr::defer({

  })
  learners = lrns(c("regr.rpart", "regr.featureless"))

})

learner = lrn("regr.bart")

publish(learner)
