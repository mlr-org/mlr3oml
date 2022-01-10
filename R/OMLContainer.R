#' @title OpenML Container
#'
#' @description
#' Contains multiple OML objects and allows to convert them into a list of [mlr3] objects.
#' @references
#' `r format_bib("vanschoren2014")`

#' @export
OMLDictionaryTask = R6Class("OMLDictionaryTask",
  # TODO: Also provide access via names
  public = list(
    items = NULL,
    initialize = function() {
      self$items = new.env(parent = emptyenv())
    },
    #' @description get an element
    #' @param key Can either be the name (data) of the task or the id.
    #' @param convert Whether the resulting object
    get = function(key, convert = FALSE) {
      if (test_integerish(key)) { # name was passed
        key = private$.id2name[[as.integer(key)]]
      } else {
        assert_character(key)
      }
      item = self$items[[key]]
      if (convert) item = item$convert()
      return(item)
    },
    keys = function() {
      names(self$items)
    },
    mget = function(keys, convert = TRUE) {
      if (is.null(keys)) keys = self$keys
      map(keys, self$get, convert = convert)
    },
    add = function(task) {
      assert_r6(task, "OMLTask")
      # TODO: pay attention with duplicate names
      # If there are name clashes append the id to the name
      name = task$data$name
      id = task$id
      assert_false(name %in% self$keys())
      assert_false(id %in% names(private$.id2name))
      assign(x = name, value = task, envir = self$items)
      private$.id2name[[id]] = name
      invisible(self)
    }
  ),
  private = list(
    .id2name = list()
  )
)

#' @export
as.data.table.OMLDictionaryTasks = function(x, ...) {
  ttt = list( # task type translator
    "Supervised Regression" = "regr",
    "Supervised Classification" = "classif"
  )
  g = function(key) {
    t = tryCatch(x$get(key), missingDefaultError = function(e) NULL)
    if (is.null(t)) {
      return(list(key = key))
    }
    l = list(
      id = t$id,
      name = t$data$name,
      task_type = ttt[t$task_type],
      nrow = as.integer(t$data$quality("NumberOfInstances")),
      ncol = t$data$quality("NumberOfFeatures"),
      nas = t$data$quality("NumberOfMissingValues"),
      num = t$data$quality("NumberOfNumericFeatures"),
      sym = t$data$quality("NumberOfSymbolicFeatures"),
      bin = t$data$quality("NumberOfBinaryFeatures")
    )
    l
  }
  setkeyv(map_dtr(x$keys(), g, .fill = TRUE), "id")[]
}


OMLContainer = R6::R6Class("OMLContainer",
  public = list(
    objects = NULL,
    contains = NULL,
    initialize = function(objects) {
      assert_list(objects)
      cls = unique(mlr3misc::map_chr(objects, function(x) class(x)[1]))
      assert_true(length(cls) == 1)
      assert_choice(cls, choices = paste0("OML", c("Task", "Data", "Run", "Flow", "Resampling")))
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

OMLDataContainer = R6::R6Class("OMLTaskContainer",
  public = list()
)


# OMLTasks = R6::R6Class("OMLTasks",
#   inherit = OMLContainer,
#   convert = function(type = "task") {
#     assert_choice(type, choices = c("task", "resampling"))
#     mlr_objects = switch(type
#       "task" = mlr3misc::map(self$objects, function(x) eval(expression(x$convert()))),
#       "resampling" = mlr3misc::map()
#
#
#     )
#     if (type == "task") {
#       mlr3_objects =     }
#     return(mlr3_objects)
#
#   }
# )# )# )# )
