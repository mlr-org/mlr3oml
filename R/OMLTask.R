#' @export
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
      self$description$task_name
    },

    description = function() {
      if (is.null(private$.description)) {
        private$.description = cached(download_task_description, "task_description", self$id, use_cache = self$use_cache)
     }

      private$.description
    },

    data_id = function() {
      as.integer(self$description$input$source_data$data_set_id)
    },

    data = function() {
      if (is.null(private$.data)) {
        private$.data = OMLData$new(self$data_id, use_cache = self$use_cache)
      }

      private$.data
    },

    nrow = function() {
      self$data$nrow
    },

    ncol = function() {
      self$data$ncol
    },

    target_names = function() {
      self$description$input$source_data$target_feature
    },

    feature_names = function() {
      setdiff(self$data$features$name, self$target_names)
    },

    task = function() {
      switch(self$description$task_type,
        # FIXME: positive class?
        "Supervised Classification" = TaskClassif$new(self$name, self$data$data, target = self$target_names),
        "Supervised Regression" = TaskRegr$new(self$name, self$data$data, target = self$target_names)
      )
    },

    resampling = function() {
      if (is.null(private$.resampling)) {
        splits = cached(download_data_splits, "data_splits", self$id, self$description, use_cache = self$use_cache)
        train_sets = splits[type == "TRAIN", list(row_id = list(rowid + 1L)), keyby = c("repeat.", "fold")]$row_id
        test_sets = splits[type == "TEST", list(row_id = list(rowid + 1L)), keyby = c("repeat.", "fold")]$row_id

        resampling = ResamplingCustom$new()
        private$.resampling = resampling$instantiate(self$task, train_sets = train_sets, test_sets = test_sets)
      }

      private$.resampling
    },

    tags = function() {
      self$description$tag
    }

  ),

  private = list(
    .data = NULL,
    .description = NULL,
    .resampling = NULL
  )
)
