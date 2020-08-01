lg = lgr::get_logger("mlr3oml")
old_threshold = lg$threshold
lg$set_threshold("warn")

..old_opts = options(mlr3oml.verbose = FALSE)
