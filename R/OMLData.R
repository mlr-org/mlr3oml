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
      self$description$name
    },

    description = function() {
      if (is.null(private$.description)) {
        private$.description = cached(download_data_description, "data_description", self$id, use_cache = self$use_cache)
     }
      private$.description
    },

    qualities = function() {
      if (is.null(private$.qualities)) {
        private$.qualities = cached(download_data_qualities, "data_qualities", self$id, use_cache = self$use_cache)
      }
      private$.qualities
    },

    features = function() {
      if (is.null(private$.features)) {
        private$.features = cached(download_data_features, "data_features", self$id, use_cache = self$use_cache)
      }
      private$.features
    },

    data = function() {
      if (is.null(private$.data)) {
        private$.data = cached(download_data, "data", self$id, description = self$description, use_cache = self$use_cache)
      }

      private$.data
    },

    target_names = function() {
      self$description$default_target_attribute
    },

    nrow = function() {
      as.integer(self$qualities[.("NumberOfInstances")]$value)
    },

    ncol = function() {
      as.integer(self$qualities[.("NumberOfFeatures")]$value)
    },

    task = function() {
      target = self$target_names
      switch(self$features[.(target), data_type],
        "nominal" = mlr3::TaskClassif$new(self$name, self$data, target = target),
        "numeric" = mlr3::TaskRegr$new(self$name, self$data, target = target),
        stop("Unknown task type")
      )
    }
  ),

  private = list(
    .data = NULL,
    .description = NULL,
    .qualities = NULL,
    .features = NULL
  )
)

download_data_description = function(id) {
  jsonlite::fromJSON(sprintf("https://www.openml.org/api/v1/json/data/%i", id))[[1L]]
}

download_data_qualities = function(id) {
  qualities = jsonlite::fromJSON(sprintf("https://www.openml.org/api/v1/json/data/qualities/%i", id))[[1L]][[1L]]
  qualities$value = as.numeric(qualities$value)
  setDT(qualities, key = "name")[]
}

download_data_features = function(id) {
  # convert_type(description$features, type_map_data_features)
  features = jsonlite::fromJSON(sprintf("https://www.openml.org/api/v1/json/data/features/%i", id))[[1L]][[1L]]
  features$is_target = as.logical(features$is_target)
  features$is_ignore = as.logical(features$is_ignore)
  features$is_row_identifier = as.logical(features$is_row_identifier)
  features$numer_of_missing_values = as.integer(features$number_of_missing_values)

  setDT(features, key = "name")[]
}

download_data = function(id, description = download_data_description(id)) {
  path = file.path(dirname(tempdir()), sprintf("oml_data_%i.arff", id))
  download.file(description$url, path, quiet = !getOption("mlr3oml.verbose", TRUE))
  on.exit(file.remove(path))

  data = read_arff(path)
  remove_named(data, c(description$row_id_attribute, description$ignore_attribute))
}
