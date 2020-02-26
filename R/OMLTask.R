OMLTask = R6Class("OMLTask",
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
      self$info$source$name
    },

    info = function() {
      if (is.null(private$.info)) {
        private$.info = download_task_info(self$id)
     }

      private$.info
    },

    data = function() {
      if (is.null(private$.data)) {
        private$.data = OMLData$new(self$info$source$data_id)
      }

      private$.data
    },

    target_names = function() {
      self$info$target_feature
    },

    task = function() {
      info = self$info
      switch(self$info$tasktype$name,
        "Supervised Classification" = mlr3::TaskClassif$new(self$name, self$data$data, target = self$target_names),
        "Supervised Regression" = mlr3::TaskRegr$new(self$name, self$data$data, target = self$target_names)
      )
    },

    resampling = function() {
      stop("'data_splits_url' missing in JSON")
    }
  ),

  private = list(
    .data = NULL,
    .info = NULL
  )
)

download_task_info = function(id) {
  info = jsonlite::fromJSON(sprintf("https://www.openml.org/t/%i/json", id))
  info$source$data_id = as.integer(info$source$data_id)
  info
}

if (FALSE) {
  self = OMLTask$new(31)
  self$data
  self$info
  self$task
}
