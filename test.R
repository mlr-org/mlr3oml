list_oml_data(limit = 10)
list_oml_data(limit = 10, number_features = "10..100")
data = get_oml_data(208)
head(data)

list_oml_tasks(limit = 10)
task = get_oml_task(10)
lrn = mlr3::lrn("classif.rpart")$train(task)
