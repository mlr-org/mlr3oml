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
        private$.info = cached(download_task_info, "task_info", self$id, use_cache = self$use_cache)
     }

      private$.info
    },

    data = function() {
      if (is.null(private$.data)) {
        private$.data = OMLData$new(self$info$source$data_id, use_cache = self$use_cache)
      }

      private$.data
    },

    target_names = function() {
      self$info$target_feature
    },

    task = function() {
      info = self$info
      switch(self$info$tasktype$name,
        # FIXME: positive class?
        "Supervised Classification" = mlr3::TaskClassif$new(self$name, self$data$data, target = self$target_names),
        "Supervised Regression" = mlr3::TaskRegr$new(self$name, self$data$data, target = self$target_names)
      )
    },

    resampling = function() {
      stop("'data_splits_url' missing in JSON")
    },

    tags = function() {
      self$info$tags$tag
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

OMLTaskConnector = function(id, use_cache = getOption("mlr3oml.use_cache", FALSE)) {
  OMLTask$new(id, use_cache)$task
}

if (FALSE) {
  self = OMLTask$new(31, use_cache = TRUE)
  self$data
  self$info
  self$task

  self = OMLTask$new(4734, use_cache = TRUE)
  self$data
  self$info
  self$task

  mlr3::tsk("oml", id = 31, use_cache = TRUE)
}
