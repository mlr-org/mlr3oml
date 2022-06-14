function(path, hash, extension) {
  assert_string(path)
  parent = switch(path, `:temp:` = tempdir(), `:user:` = R_user_dir("mlr 3db", "cache"), path)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE)
  }
  file.path(parent, sprintf("%s.%s", gsub("[^[:alnum:]._-]",
    "_", hash), extension))
}
