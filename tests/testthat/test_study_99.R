context("study_99")


test_that("study 99 can be loaded and parsed", {
  skip("Expensive manual test")

  data_ids = list_oml_data(tag = "study_99")$did
  use_cache = FALSE

  for (data_id in data_ids) {
    data_id = 40984L

    odata = OMLData$new(data_id, use_cache = use_cache)
    expect_count(odata$id)
    expect_identical(odata$id, data_id)
    expect_flag(odata$use_cache)

    expect_string(odata$name, min.chars = 1L)
    expect_list(odata$description)
    expect_identical(odata$description$id, data_id)

    data = odata$data
    expect_data_table(data, nrows = odata$nrow, ncols = odata$ncol)
    expect_names(odata$feature_names, "strict")
    expect_names(odata$target_names, "strict")
    expect_disjunct(odata$target_names, odata$feature_names)
    expect_set_equal(names(data), c(odata$feature_names, odata$target_names))

    qualities = odata$qualities
    expect_data_table(qualities, ncols = 2L, key = "name", min.rows = 2L)
    expect_character(qualities$name, any.missing = FALSE, min.chars = 1L)
    expect_numeric(qualities$value, any.missing = TRUE)
    expect_subset(c("NumberOfFeatures", "NumberOfInstances"), qualities$name)

    features = odata$features
    expect_data_table(features, min.cols = 8L, key = "index")
    expect_integer(features$index, lower = 0L, upper = nrow(features))
    expect_subset(c(odata$target_names, odata$feature_names), features$name)
    expect_set_equal(c(odata$target_names, odata$feature_names), features[!is_row_identifier & !is_ignore, name])
    expect_equal(sum(features$is_target), 1) # only classif and regr
    expect_logical(features$is_ignore, any.missing = FALSE)
    expect_logical(features$is_row_identifier, any.missing = FALSE)
    expect_integer(features$number_of_missing_values, lower = 0L, upper = odata$nrow, any.missing = FALSE)
    expect_list(features$nominal_value, types = c("NULL", "character"))

    task = odata$task
    expect_task(odata$task)
    expect_identical(odata$nrow, task$nrow)
    expect_identical(odata$ncol, task$ncol)
    expect_set_equal(odata$feature_names, task$feature_names)
    expect_set_equal(odata$target_names, task$target_names)
  }



#   id = 46
#   description = download_data_description(id)
#   path = file.path(tempdir(), sprintf("oml_data_%i.arff", id))
#   download.file(description$url, path, quiet = !getOption("mlr3oml.verbose", TRUE))

#   mine = read_arff(path)
#   theirs = as.data.table(foreign::read.arff(path))
#   head(mine)
#   head(theirs)

#   tail(mine)
#   tail(theirs)
#   path


#   match(as.character(mine$Instance_name), as.character(theirs$Instance_name))
})
