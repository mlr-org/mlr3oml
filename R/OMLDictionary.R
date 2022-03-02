#' @title OpenML Dictionary
#' @description
#' Dictionaries for [OMLTask]s, [OMLFlow]s, [OMLData] and [OMLRun]s.
#' Can each be converted do `data.table` using `as.data.table()`.
OMLDictionary = R6Class("OMLDictionary",
  public = list(
    #' @field items (`environment`)
    #'   Items of the dictionary.
    items = NULL,
    #' @description
    #' Initializes an instance of class OMLDictionary.
    initialize = function() {
      self$items = new.env(parent = emptyenv())
    },
    #' @description
    #' Get an element from the dictionary using its ID.
    #' @param key (`integer(1)`) ID of the OpenML object.
    #' @param convert (`logical(1)`) Whether to convert the resulting object to an mlr3 object.
    get = function(key, convert = FALSE) {
      key = as.character(assert_integerish(key, coerce = TRUE, len = 1))
      assert_flag(convert)
      item = self$items[[key]]
      if (convert) {
        item = item$convert()
      }
      return(item)
    },
    #' @description
    #' Prints the object.
    print = function() {
      print(as.data.table(self))
    },
    #' @description
    #' Returns the OpenML IDs, which are the keys for the dictionary.
    keys = function() {
      as.integer(names(self$items))
    },
    #' @description
    #' Retrieves multiple items from the dictionary.
    #' @param keys (`integer`) IDs of the OpenML object.
    #' @param convert (`logical(1)`) Whether to convert the resulting object to an mlr3 object.
    mget = function(keys = NULL, convert = FALSE) {
      if (is.null(keys)) keys = self$keys()
      keys = assert_integerish(keys, coerce = TRUE)
      map(keys, self$get, convert = convert)
    },
    #' @description
    #' Adds An OML object to the dictionary.
    #' @param x An OpenML object (specified by the subclass).
    add = function(x) {
      assert_r6(x, private$.type)
      id = as.character(x$id)
      assert_false(id %in% self$keys())
      assign(x = id, value = x, envir = self$items)
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
    get_rsmp = function(key, convert = FALSE) {
      oml_task = self$get(key, convert = FALSE)
      item = oml_task$resampling
      if (convert) item = item$convert()
      return(item)
    },
    mget_rsmp = function(keys = NULL, convert = FALSE) {
      if (is.null(keys)) {
        keys = self$keys()
      }
      map(keys, self$get_rsmp, convert = convert)
    }
  ),
  private = list(
    .type = "OMLTask"
  )
)

OMLDictionaryData = R6Class("OMLDictionaryData",
  inherit = OMLDictionary,
  private = list(
    .type = "OMLData"
  )
)

OMLDictionaryFlow = R6Class("OMLDictionaryFlow",
  inherit = OMLDictionary,
  private = list(
    .type = "OMLFlow"
  )
)

OMLDictionaryRun = R6Class("OMLDictionaryRun",
  inherit = OMLDictionary,
  private = list(
    .type = "OMLRun"
  )
)


#' @export
as.data.table.OMLDictionaryTask = function(x, ...) { # nolint
  # TODO: What about resampling?
  eptt = list( # estimation procedure type translation
    crossvalidation = "cv",
    holdout = "ho"
  )
  g = function(key) {
    t = tryCatch(x$get(key, convert = FALSE), missingDefaultError = function(e) NULL)
    if (is.null(t)) {
      return(list(key = key))
    }
    list(
      id = t$id,
      data = t$data$name,
      task_type = task_type_translator[[t$task_type]],
      target = t$target_names, # can have length > 1
      nrow = as.integer(t$data$quality("NumberOfInstances")),
      ncol = t$data$quality("NumberOfFeatures"),
      nas = t$data$quality("NumberOfMissingValues"),
      num = t$data$quality("NumberOfNumericFeatures"),
      sym = t$data$quality("NumberOfSymbolicFeatures"),
      bin = t$data$quality("NumberOfBinaryFeatures"),
      rsmp = eptt[[t$resampling$estimation_procedure]]
    )
  }
  setkeyv(map_dtr(x$keys(), g, .fill = TRUE), "id")[]
}


#' @export
as.data.table.OMLDictionaryFlow = function(x, ...) { # nolint
  g = function(key) {
    t = tryCatch(x$get(key, convert = FALSE), missingDefaultError = function(e) NULL)
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


#' @export
as.data.table.OMLDictionaryData = function(x, ...) { # nolint
  g = function(key) {
    t = tryCatch(x$get(key, convert = FALSE), missingDefaultError = function(e) NULL)
    if (is.null(t)) {
      return(list(key = key))
    }
    list(
      id = t$id,
      name = t$name,
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
    t = tryCatch(x$get(key, convert = FALSE), missingDefaultError = function(e) NULL)
    if (is.null(t)) {
      return(list(key = key))
    }
    list(
      id = t$id,
      type = task_type_translator[[t$task_type]],
      task = t$desc$input_data$dataset$name,
      flow = t$desc$flow_name
    )
  }
  setkeyv(map_dtr(x$keys(), g, .fill = TRUE), "id")[]
}
