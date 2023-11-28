library(checkmate)
lapply(list.files(system.file("testthat", package = "mlr3"),
  pattern = "^helper.*\\.[rR]$",
  full.names = TRUE
), source)


lg = lgr::get_logger("mlr3oml")
old_threshold = lg$threshold
lg$set_threshold("warn")

..old_opts = options(
  mlr3oml.verbose = FALSE,
  mlr3oml.retries = 20L,
  mlr3oml.cache = if (identical(Sys.getenv("GITHUB_ACTIONS"), "true")) "~/openml" else FALSE
)
