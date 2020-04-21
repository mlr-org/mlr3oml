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

    feature_names = function() {
      setdiff(self$features$name, self$target_names)
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
        "nominal" = TaskClassif$new(self$name, self$data, target = target),
        "numeric" = TaskRegr$new(self$name, self$data, target = target),
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
