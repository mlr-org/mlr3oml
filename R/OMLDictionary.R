#' @title OpenML Dictionary
#' @description
#' @export
#' @example
OMLDictionary = R6Class("OMLDictionary",
  public = list(
    items = NULL,
    initialize = function() {
      self$items = new.env(parent = emptyenv())
    },
    #' @description get an element
    #' @param key Can either be the name (data) of the task or the id.
    #' @param convert Whether the resulting object
    get = function(key, convert = TRUE) {
      key = as.character(assert_integerish(key, coerce = TRUE))
      item = self$items[[key]]
      if (convert) {
        item = item$convert()
      }
      return(item)
    },
    #'
    keys = function() {
      as.integer(names(self$items))
    },
    mget = function(keys = NULL, convert = TRUE) {
      if (is.null(keys)) keys = self$keys()
      map(keys, self$get, convert = convert)
    },
    add = function(task) {
      assert_r6(task, private$.type)
      id = as.character(task$id)
      assert_false(id %in% self$keys())
      assign(x = id, value = task, envir = self$items)
      invisible(self)
    }
  ),
  private = list(
    .type = NULL
  )
)

#' @export
OMLDictionaryTask = R6Class("OMLDictionaryTask",
  inherit = OMLDictionary,
  public = list(
    get_rsmp = function(key, convert = TRUE) {
      oml_task = self$get(key, convert = FALSE)
      item = oml_task$resampling
      if (convert) item = item$convert()
      return(item)
    },
    mget_rsmp = function(key, convert = TRUE) {
      if (is.null(keys)) keys = self$keys()
      map(keys, self$get_rsmp, convert = convert)
    }
  ),
  private = list(
    .type = "OMLTask"
  )
)

#' @export
OMLDictionaryData = R6Class("OMLDictionaryData",
  inherit = OMLDictionary,
  private = list(
    .type = "OMLData"
  )
)

#' @export
OMLDictionaryFlow = R6Class("OMLDictionaryFlow",
  inherit = OMLDictionary,
  private = list(
    .type = "OMLFlow"
  )
)

#' @export
OMLDictionaryRun = R6Class("OMLDictionaryRun",
  inherit = OMLDictionary,
  private = list(
    .type = "OMLRun"
  )
)



#' @export
as.data.table.OMLDictionaryTask = function(x, ...) { # nolint
  # TODO: What about resampling?
  ttt = list( # task type translator
    "Supervised Regression" = "regr",
    "Supervised Classification" = "classif"
  )
  eptt = list( # estimatino procedure type translation
    crossvalidation = "cv",
    holdout = "ho"
  )
  g = function(key) {
    t = tryCatch(x$get(key), missingDefaultError = function(e) NULL)
    if (is.null(t)) {
      return(list(key = key))
    }
    list(
      id = t$id,
      data = truncate_name(t$data$name, width = 15L),
      task_type = ttt[[t$task_type]],
      target = truncate_vector(t$target_names), # can have length > 1
      nrow = as.integer(t$data$quality("NumberOfInstances")),
      ncol = t$data$quality("NumberOfFeatures"),
      nas = t$data$quality("NumberOfMissingValues"),
      num = t$data$quality("NumberOfNumericFeatures"),
      sym = t$data$quality("NumberOfSymbolicFeatures"),
      bin = t$data$quality("NumberOfBinaryFeatures"),
      rsmp = eptt[[t$resampling$estimation_procedure$type]]
    )
  }
  setkeyv(map_dtr(x$keys(), g, .fill = TRUE), "id")[]
}


#' @export
as.data.table.OMLDictionaryFlow = function(x, ...) { # nolint
  g = function(key) {
    t = tryCatch(x$get(key), missingDefaultError = function(e) NULL)
    if (is.null(t)) {
      return(list(key = key))
    }
    list(
      id = t$id,
      name = truncate_name(t$name)
    )
  }
  setkeyv(map_dtr(x$keys(), g, .fill = TRUE), "id")[]
}

#' @export
as.data.table.OMLDictionaryData = function(x, ...) { # nolint
  g = function(key) {
    t = tryCatch(x$get(key), missingDefaultError = function(e) NULL)
    if (is.null(t)) {
      return(list(key = key))
    }
    list(
      id = t$id,
      name = truncate_name(t$name),
      nrow = as.integer(t$quality("NumberOfInstances")),
      ncol = t$quality("NumberOfFeatures"),
      nas = t$quality("NumberOfMissingValues"),
      num = t$quality("NumberOfNumericFeatures"),
      sym = t$quality("NumberOfSymbolicFeatures"),
      bin = t$quality("NumberOfBinaryFeatures")
    )
  }
  setkeyv(map_dtr(x$keys(), g, .fill = TRUE), "id")[]
}

#' @export
as.data.table.OMLDictionaryRun = function(x, ...) { # nolint
  g = function(key) {
    t = tryCatch(x$get(key), missingDefaultError = function(e) NULL)
    if (is.null(t)) {
      return(list(key = key))
    }
    list(
      id = t$id,
      name = t$name
    )
  }
  setkeyv(map_dtr(x$keys(), g, .fill = TRUE), "id")[]
}
