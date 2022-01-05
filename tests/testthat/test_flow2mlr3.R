test_that("Can construct pseudo learner", {
  ll = mlr3extralearners::list_mlr3learners(select = c(
    "mlr3_package", "id", "required_packages",
    "name", "class"
  ))
  ll = ll[class %in% c("classif", "regr", "surv")]

  oml_list = list(
    "oml_name" = sprintf("mlr3.%s", ll$id),
    "dependencies" = ll$required_packages,
    "class" = ll$class,
    "name" = ll$name
  )

  for (i in seq_along(nrow(ll))) {
    # Create pseudoflow
    flow = list(name = oml_list$oml_name[[i]], dependencies = oml_list$dependencies[[i]])
    lrn = flow2mlr3(flow)
    expect_learner(lrn)
  }
})

test_that("Can construct actual learner", {

})

flow = OMLFlow$new(14744)
