OMLData = R6Class("OMLData",
  public = list(
    id = NULL,
    use_cache = NULL,

    initialize = function(id, use_cache = getOption("mlr3oml.use_cache", FALSE)) {
      self$id = assert_count(id, coerce = TRUE)
      self$use_cache = assert_flag(use_cache)
    }
  ),

  active = list(
    name = function() {
      self$info$name
    },

    info = function() {
      if (is.null(private$.info)) {
        private$.info = download_data_info(self$id)
     }

      private$.info
    },

    data = function() {
      if (is.null(private$.data)) {
        private$.data = download_data(self$id, self$info)
      }

      private$.data
    },

    target_names = function() {
      self$info$default_target_attribute
    },

    nrow = function() {
      self$info$qualities$NumberOfInstances
    },

    ncol = function() {
      self$info$qualities$NumberOfFeatures
    },

    task = function() {
      target = self$target_names
      switch(self$info$features[list(target), type],
        "nominal" = mlr3::TaskClassif$new(self$name, self$data, target = target),
        stop("Unknown task type")
      )
    }
  ),

  private = list(
    .data = NULL,
    .qualities = NULL,
    .features = NULL,
    .info = NULL
  )
)

download_data_info = function(id) {
  info = jsonlite::fromJSON(sprintf("https://www.openml.org/d/%i/json", id))

  setDT(info$features, key = "name")
  convert_type(info$features, type_map_data_features)
  return(info)
}

download_data = function(id, info = download_data_info(id)) {
  path = file.path(dirname(tempdir()), sprintf("oml_data_%i.arff", id))
  download.file(info$url, path)
  # on.exit(file.remove(path))
  data = read_arff(path)

  remove_named(data, c(info$row_id_attribute, info$ignore_attribute))
}

if (FALSE) {
  self = OMLData$new(1510)
  self$nrow
  self$ncol
  ncol(self$data)
  self = OMLData$new(1038)
  self$data
  self$task

  # new api:
  jsonlite::fromJSON("https://www.openml.org/api/v1/json/data/57")
  jsonlite::fromJSON("https://www.openml.org/api/v1/json/data/qualities/57")
  jsonlite::fromJSON("https://www.openml.org/api/v1/json/data/features/57")
}
