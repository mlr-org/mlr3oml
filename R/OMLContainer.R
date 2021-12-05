OMLContainer = R6::R6Class("OMLContainer",
  public = list(
    objects = NULL,
    contains = NULL,
    initialize = function(objects) {
      assert_list(objects)
      cls = unique(mlr3misc::map_chr(objects, function(x) class(x)[1]))
      assert_true(length(cls) == 1)
      assert_choice(cls, choices = paste0("OML", c("Task", "Data", "Run", "Flow")))
      self$contains = cls
      self$objects = objects
    },
    convert = function() {
      mlr3_objects = mlr3misc::map(self$objects, function(x) eval(expression(x$convert())))
      return(mlr3_objects)
    },
    print = function() {
      catf("<OMLContainer: %s (%i)>", self$contains, length(self$objects))
    },
    get = function(i) {
      return(self$objects[[i]])
    },
    set = function(i, x) {
      self$objects[[i]] = x
    }
  )
)


