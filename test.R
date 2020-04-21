library(mlr3)

tsk("oml", 59)
tsk("oml", data_id = 12)
rsmp("oml", task_id = 59)

TaskClassif$new("iris", OMLData$new(16), target = "class")
