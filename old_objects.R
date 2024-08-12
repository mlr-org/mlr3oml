library(mlr3oml)

odata = odt(61)
odata$download()
saveRDS(odata, file.path("./inst/old/odata61.rds"))

otask = otsk(61)
otask$download()
saveRDS(otask, file.path("./inst/old/otask61.rds"))

oflow = oflw(1)
oflow$download()
saveRDS(oflow, file.path(".inst/old/l"))


test_that("new api works for OMLData", {
  old = readRDS(system.file("old", "odata61.rds", package = "mlr3oml"))
  new = odt(61, test_server = TRUE)
  all.equal(old, new)

  fields = unique(c(names(old$desc), names(new$desc)))

  for (field in fields) {
    if (!isTRUE(all.equal(old$desc[[field]], new$desc[[field]]))) {
      print(field)
      print("old:")
      print(old$desc[[field]])
      print("new:")
      print(new$desc[[field]])
    }
  }
})

test_that("new api works for OMLTask", {
  oldtask = readRDS(system.file("old", "otask61.rds", package = "mlr3oml"))
  download_desc_task(61, get_server(FALSE))
  newdesc = get_json(paste0(get_server(FALSE), "/tasks/61"),
    simplify_data_frame = FALSE, server = get_server(FALSE)
  )
  path = tempfile()
  response = download_file(paste0(get_server(FALSE), "/tasks/61"), path, server = get_server(FALSE))
  x = readLines(path)
  olddesc = oldtask$desc
  oldtask$desc
  newdesc
  all.equal(old, new)

})
